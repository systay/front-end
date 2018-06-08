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
package org.opencypher.v9_0.rewriting.rewriters

import org.opencypher.v9_0.ast._
import org.opencypher.v9_0.expressions.{Expression, Variable}
import org.opencypher.v9_0.util._
import org.opencypher.v9_0.util.attribution.{Attributes, SameId}

import scala.collection.mutable

/**
 * This rewriter makes sure that all return items in a RETURN clauses are aliased, and moves
 * any ORDER BY to a preceding WITH clause
 *
 * Example:
 *
 * MATCH (n)
 * RETURN n.foo AS foo, n.bar ORDER BY foo
 *
 * This rewrite will change the query to:
 *
 * MATCH (n)
 * WITH n.foo AS `  FRESHIDxx`, n.bar AS `  FRESHIDnn` ORDER BY `  FRESHIDxx`
 * RETURN `  FRESHIDxx` AS foo, `  FRESHIDnn` AS `n.bar`
 */
case class normalizeReturnClauses(mkException: (String, InputPosition) => CypherException, attributes: Attributes) extends Rewriter {

  def apply(that: AnyRef): AnyRef = instance.apply(that)

  private val clauseRewriter: Clause => Seq[Clause] = {
    case clause @ Return(_, returnItems@ReturnItems(_, items), None, _, _, _) =>
      val aliasedItems = items.map({
        case i: AliasedReturnItem =>
          i
        case i =>
          val newPosition = i.expression.position.bumped()
          AliasedReturnItem(i.expression, Variable(i.name)(newPosition)(attributes.copy(i.id)))(i.position)(SameId(i.id))
      })
      val newReturnItems = returnItems.copy(items = aliasedItems)(returnItems.position)(SameId(returnItems.id))
      Seq(
        clause.copy(returnItems = newReturnItems)(clause.position)(SameId(clause.id))
      )

    case clause @ Return(distinct, returnItems: ReturnItems, orderBy, skip, limit, _) =>
      clause.verifyOrderByAggregationUse((s,i) => throw mkException(s,i))
      var rewrites = mutable.Map[Expression, Variable]()

      val (aliasProjection, finalProjection) = returnItems.items.map {
        i =>
          val returnColumn = i.alias match {
            case Some(alias) => alias
            case None        => Variable(i.name)(i.expression.position.bumped())(attributes.copy(i.id))
          }

          val newVariable = Variable(FreshIdNameGenerator.name(i.expression.position))(i.expression.position)(attributes.copy(i.id))

          // Always update for the return column, so that it has precedence over the expressions (if there are variables with the same name),
          // e.g. match (n),(m) return n as m, m as m2
          rewrites += (returnColumn -> newVariable)
          // Only update if rewrites does not yet have a mapping for i.expression
          rewrites.getOrElseUpdate(i.expression, newVariable)

          (AliasedReturnItem(i.expression, newVariable)(i.position)(attributes.copy(i.id)), AliasedReturnItem(newVariable.copyId, returnColumn)(i.position)(attributes.copy(i.id)))
      }.unzip

      val newOrderBy = orderBy.endoRewrite(topDown(Rewriter.lift {
        case exp: Expression if rewrites.contains(exp) => rewrites(exp).copyId
      }))

      val introducedVariables = if (returnItems.includeExisting) aliasProjection.map(_.variable.name).toSet else Set.empty[String]

      Seq(
        With(distinct = distinct, returnItems = returnItems.copy(items = aliasProjection)(returnItems.position)(attributes.copy(returnItems.id)),
          orderBy = newOrderBy, skip = skip, limit = limit, where = None)(clause.position)(attributes.copy(clause.id)),
        Return(distinct = false, returnItems = returnItems.copy(items = finalProjection)(returnItems.position)(attributes.copy(returnItems.id)),
          orderBy = None, skip = None, limit = None, excludedNames = introducedVariables)(clause.position)(attributes.copy(clause.id))
      )

    case clause =>
      Seq(clause)
  }

  private val instance: Rewriter = bottomUp(Rewriter.lift {
    case query @ SingleQuery(clauses) =>
      query.copy(clauses = clauses.flatMap(clauseRewriter))(query.position)(SameId(query.id))
  })
}
