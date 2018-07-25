/*
 * Copyright © 2002-2018 Neo4j Sweden AB (http://neo4j.com)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.opencypher.v9_0.frontend.phases

import org.opencypher.v9_0.ast._
import org.opencypher.v9_0.expressions.{Variable, _}
import org.opencypher.v9_0.rewriting.conditions.{aggregationsAreIsolated, hasAggregateButIsNotAggregate}
import org.opencypher.v9_0.util.attribution.{Attributes, SameId}
import org.opencypher.v9_0.util.helpers.fixedPoint
import org.opencypher.v9_0.util.{AggregationNameGenerator, InternalException, Rewriter, bottomUp, _}

/**
  * This rewriter makes sure that aggregations are on their own in RETURN/WITH clauses, so
  * the planner can have an easy time
  *
  * Example:
  *
  * MATCH (n)
  * RETURN { name: n.name, count: count(*) }, n.foo
  *
  * This query has a RETURN clause where the single expression contains both the aggregate key and
  * the aggregation expression. To make the job easier on the planner, this rewrite will change the query to:
  *
  * MATCH (n)
  * WITH n.name AS x1, count(*) AS x2, n.foo as X3
  * RETURN { name: x1, count: x2 }
  */
case object isolateAggregation extends StatementRewriter {

  override def instance(from: BaseState, context: BaseContext): Rewriter = {
    val attributes = Attributes(context.astIdGen, from.positions())
    bottomUp(rewriter(attributes), _.isInstanceOf[Expression])
  }

  override def description: String = "Makes sure that aggregations are on their own in RETURN/WITH clauses"

  override def postConditions: Set[Condition] = Set(StatementCondition(aggregationsAreIsolated))

  private def rewriter(attributes: Attributes): Rewriter = Rewriter.lift {
    case q@SingleQuery(clauses) =>

      val newClauses = clauses.flatMap {
        case clause: ProjectionClause if clauseNeedingWork(clause) =>
          val clauseReturnItems = clause.returnItems.items
          val (withAggregations, others) = clauseReturnItems.map(_.expression).toSet.partition(hasAggregateButIsNotAggregate(_))

          val expressionsToIncludeInWith: Set[Expression] = others ++ extractExpressionsToInclude(withAggregations)

          val withReturnItems: Set[ReturnItem] = expressionsToIncludeInWith.map {
            e =>
              val variable = Variable(AggregationNameGenerator.name(e.position))(e.position)(attributes.copy(e.id))
              AliasedReturnItem(e, variable)(e.position)(attributes.copy(e.id))
          }
          val returnItems = ReturnItems(includeExisting = false, withReturnItems.toIndexedSeq)(clause.position)(attributes.copy(clause.id))
          val withClause = With(distinct = false, returnItems, None, None, None, None)(clause.position)(attributes.copy(clause.id))

          val expressionRewriter = createRewriterFor(withReturnItems)
          val newReturnItems = clauseReturnItems.map {
            case ri@AliasedReturnItem(expression, _) =>
              ri.copy(expression = expression.endoRewrite(expressionRewriter))(ri.position)(attributes.copy(ri.id))
            case ri@UnaliasedReturnItem(expression, _) =>
              ri.copy(expression = expression.endoRewrite(expressionRewriter))(ri.position)(attributes.copy(ri.id))
          }
          val resultClause = clause.withReturnItems(newReturnItems)

          IndexedSeq(withClause, resultClause)

        case clause => IndexedSeq(clause)
      }

      q.copy(clauses = newClauses)(q.position)(SameId(q.id))
  }

  private def createRewriterFor(withReturnItems: Set[ReturnItem]): Rewriter = {
    def inner = Rewriter.lift {
      case original: Expression =>
        val rewrittenExpression = withReturnItems.collectFirst {
          case item@AliasedReturnItem(expression, _) if original == expression =>
            item.alias.get.copyId
        }
        rewrittenExpression getOrElse original
    }
    topDown(inner)
  }

  private def extractExpressionsToInclude(originalExpressions: Set[Expression]): Set[Expression] = {
    val expressionsToGoToWith: Set[Expression] = fixedPoint {
      expressions: Set[Expression] => expressions.flatMap {
        case e: ReduceExpression if hasAggregateButIsNotAggregate(e) =>
          Seq(e.init, e.list)

        case e: FilterExpression if hasAggregateButIsNotAggregate(e) =>
          Seq(e.expression)

        case e: ExtractExpression if hasAggregateButIsNotAggregate(e) =>
          Seq(e.expression)

        case e: ListComprehension if hasAggregateButIsNotAggregate(e) =>
          Seq(e.expression)

        case e: DesugaredMapProjection if hasAggregateButIsNotAggregate(e) =>
          e.items.map(_.exp) :+ e.name

        case e: IterablePredicateExpression  if hasAggregateButIsNotAggregate(e) =>
          val predicate: Expression = e.innerPredicate.getOrElse(throw new InternalException("Should never be empty"))
          // Weird way of doing it to make scalac happy
          Set(e.expression) ++ predicate.dependencies - e.variable

        case e if hasAggregateButIsNotAggregate(e) =>
          e.arguments

        case e =>
          Seq(e)
      }
    }(originalExpressions).filter {
      //Constant expressions should never be isolated
      expr => IsAggregate(expr) || expr.dependencies.nonEmpty
    }
    expressionsToGoToWith
  }

  private def clauseNeedingWork(c: Clause): Boolean = c.treeExists {
    case e: Expression => hasAggregateButIsNotAggregate(e)
  }
}
