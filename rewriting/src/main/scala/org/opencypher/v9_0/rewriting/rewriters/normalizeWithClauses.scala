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

import org.opencypher.v9_0.ast.{Where, _}
import org.opencypher.v9_0.expressions.{Expression, LogicalVariable, Variable}
import org.opencypher.v9_0.util._
import org.opencypher.v9_0.util.attribution.{Attributes, SameId}

/**
 * This rewriter normalizes the scoping structure of a query, ensuring it is able to
 * be correctly processed for semantic checking. It makes sure that all return items
 * in a WITH clauses are aliased, and ensures all ORDER BY and WHERE expressions are
 * shifted into the clause, leaving only a variable. That variable must also
 * appear as an alias in the associated WITH.
 *
 * This rewriter depends on normalizeReturnClauses having first been run.
 *
 * Example:
 *
 * MATCH n
 * WITH n.prop AS prop ORDER BY n.foo DESC
 * RETURN prop
 *
 * This rewrite will change the query to:
 *
 * MATCH n
 * WITH n AS n, n.prop AS prop
 * WITH prop AS prop, n.foo AS `  FRESHID39` ORDER BY `  FRESHID39` DESC
 * WITH prop AS prop
 * RETURN prop
 *
 * It uses multiple WITH clauses to ensure that cardinality and grouping are not altered, even in the presence
 * of aggregation.
 */
case class normalizeWithClauses(mkException: (String, InputPosition) => CypherException, attributes: Attributes) extends Rewriter {

  def apply(that: AnyRef): AnyRef = instance.apply(that)

  private val clauseRewriter: Clause => Seq[Clause] = {
    case clause @ With(_, returnItems: ReturnItems, None, _, _, None) =>
      val (unaliasedReturnItems, aliasedReturnItems) = partitionReturnItems(returnItems.items)
      val initialReturnItems = unaliasedReturnItems ++ aliasedReturnItems
      Seq(clause.copy(returnItems = returnItems.copy(items = initialReturnItems)(returnItems.position)(SameId(returnItems.id)))(clause.position)(SameId(clause.id)))

    case clause @ With(distinct, returnItems: ReturnItems, orderBy, skip, limit, where) =>
      clause.verifyOrderByAggregationUse((s,i) => throw mkException(s,i))
      val (unaliasedReturnItems, aliasedReturnItems) = partitionReturnItems(returnItems.items)
      val initialReturnItems = unaliasedReturnItems ++ aliasedReturnItems
      val (introducedReturnItems, updatedOrderBy, updatedWhere) = aliasOrderByAndWhere(aliasedReturnItems.map(i => i.expression -> i.alias.get.copyId).toMap, orderBy, where)
      val requiredVariablesForOrderBy = updatedOrderBy.map(_.dependencies).getOrElse(Set.empty) diff (introducedReturnItems.map(_.variable).toSet ++ initialReturnItems.flatMap(_.alias))

      if (orderBy == updatedOrderBy && where == updatedWhere) {
        Seq(clause.copy(returnItems = returnItems.copy(items = initialReturnItems)(returnItems.position)(SameId(returnItems.id)))(clause.position)(SameId(clause.id)))
      } else if (introducedReturnItems.isEmpty) {
        Seq(clause.copy(returnItems = returnItems.copy(items = initialReturnItems)(returnItems.position)(SameId(returnItems.id)), orderBy = updatedOrderBy, where = updatedWhere)(clause.position)(SameId(clause.id)))
      } else {
        val secondProjection = if (returnItems.includeExisting) {
          introducedReturnItems
        } else {

          initialReturnItems.map((item: ReturnItem) =>
            item.alias.fold(item)(alias => AliasedReturnItem(alias.copyId, alias.copyId)(item.position)(attributes.copy(item.id)))
          ) ++
            requiredVariablesForOrderBy.toIndexedSeq.map(i => AliasedReturnItem(i.copyId, i.copyId)(i.position)(attributes.copy(i.id))) ++
            introducedReturnItems
        }

        val firstProjection = if (distinct || returnItems.containsAggregate || returnItems.includeExisting) {
          initialReturnItems
        } else {
          val requiredReturnItems = introducedReturnItems.flatMap(_.expression.dependencies).toSet diff initialReturnItems
            .flatMap(_.alias).toSet
          val requiredVariables = requiredReturnItems ++ requiredVariablesForOrderBy

          requiredVariables.toIndexedSeq.map(i => AliasedReturnItem(i.copyId, i.copyId)(i.position)(attributes.copy(i.id))) ++ initialReturnItems
        }

        val introducedVariables = introducedReturnItems.map(_.variable.copyId)

        Seq(
          With(distinct = distinct, returnItems = returnItems.copy(items = firstProjection)(returnItems.position)(attributes.copy(returnItems.id)),
            orderBy = None, skip = None, limit = None, where = None)(clause.position)(attributes.copy(clause.id)),
          With(distinct = false, returnItems = returnItems.copy(items = secondProjection)(returnItems.position)(attributes.copy(returnItems.id)),
            orderBy = updatedOrderBy, skip = skip, limit = limit, where = updatedWhere)(clause.position)(attributes.copy(clause.id)),
          PragmaWithout(introducedVariables)(clause.position)(attributes.copy(clause.id))
        )
      }

    case clause =>
      Seq(clause)
  }

  private def partitionReturnItems(returnItems: Seq[ReturnItem]): (Seq[ReturnItem], Seq[AliasedReturnItem]) =
    returnItems.foldLeft((Vector.empty[ReturnItem], Vector.empty[AliasedReturnItem])) {
      case ((unaliasedItems, aliasedItems), item) => item match {
        case i: AliasedReturnItem =>
          (unaliasedItems, aliasedItems :+ i)

        case i if i.alias.isDefined =>
          (unaliasedItems, aliasedItems :+ AliasedReturnItem(item.expression, item.alias.get.copyId)(item.position)(attributes.copy(item.id)))

        case _ =>
          // Unaliased return items in WITH will be preserved so that semantic check can report them as an error
          (unaliasedItems :+ item, aliasedItems)
      }
    }

  private def aliasOrderByAndWhere(existingAliases: Map[Expression, LogicalVariable], orderBy: Option[OrderBy], where: Option[Where]): (Seq[AliasedReturnItem], Option[OrderBy], Option[Where]) = {
    val (additionalReturnItemsForOrderBy, updatedOrderBy) = orderBy match {
      case Some(o) =>
        val (returnItems, updatedOrderBy) = aliasOrderBy(existingAliases, o)
        (returnItems, Some(updatedOrderBy))

      case None =>
        (Seq.empty, None)
    }

    val (maybeReturnItemForWhere, updatedWhere) = where match {
      case Some(w) =>
        val (maybeReturnItem, updatedWhere) = aliasWhere(existingAliases, w)
        (maybeReturnItem, Some(updatedWhere))

      case None =>
        (None, None)
    }

    (additionalReturnItemsForOrderBy ++ maybeReturnItemForWhere, updatedOrderBy, updatedWhere)
  }

  private def aliasOrderBy(existingAliases: Map[Expression, LogicalVariable], originalOrderBy: OrderBy): (Seq[AliasedReturnItem], OrderBy) = {
    val (additionalReturnItems, updatedSortItems) = originalOrderBy.sortItems.foldLeft((Vector.empty[AliasedReturnItem], Vector.empty[SortItem])) {
      case ((returnItems, sortItems), item) => item.expression match {
        case _: Variable =>
          (returnItems, sortItems :+ item)

        case e: Expression =>
          val (maybeReturnItem, sortItem) = aliasSortItem(existingAliases, item)
          maybeReturnItem match {
            case Some(i) if !i.expression.containsAggregate =>
              (returnItems :+ i, sortItems :+ sortItem)
            case Some(i) =>
              (returnItems, sortItems :+ item)
            case None =>
              (returnItems, sortItems :+ sortItem)
          }
      }
    }
    (additionalReturnItems, OrderBy(updatedSortItems)(originalOrderBy.position)(attributes.copy(originalOrderBy.id)))
  }

  private def aliasSortItem(existingAliases: Map[Expression, LogicalVariable], sortItem: SortItem): (Option[AliasedReturnItem], SortItem) = {
    val expression = sortItem.expression
    val (maybeReturnItem, replacementVariable) = aliasExpression(existingAliases, expression)

    val newSortItem = sortItem.endoRewrite(topDown(Rewriter.lift {
      case e: Expression if e == expression =>
        replacementVariable.copyId
    }))
    (maybeReturnItem, newSortItem)
  }

  private def aliasWhere(existingAliases: Map[Expression, LogicalVariable], originalWhere: Where): (Option[AliasedReturnItem], Where) = {
    originalWhere.expression match {
      case _: Variable =>
        (None, originalWhere)

      case e: Expression if !e.containsAggregate =>
        val (maybeReturnItem, replacementVariable) = aliasExpression(existingAliases, e)
        (maybeReturnItem, Where(replacementVariable)(originalWhere.position)(attributes.copy(originalWhere.id)))

      case e =>
        (None, originalWhere)
    }
  }

  private def aliasExpression(existingAliases: Map[Expression, LogicalVariable], expression: Expression): (Option[AliasedReturnItem], LogicalVariable) = {
    existingAliases.get(expression) match {
      case Some(alias) =>
        (None, alias.copyId)

      case None =>
        val newVariable = Variable(FreshIdNameGenerator.name(expression.position))(expression.position)(attributes.copy(expression.id))
        val newExpression = expression.endoRewrite(topDown(Rewriter.lift {
          case e: Expression =>
            existingAliases.get(e).map(_.copyId).getOrElse(e)
        }))
        val newReturnItem = AliasedReturnItem(newExpression, newVariable)(expression.position)(attributes.copy(expression.id))
        (Some(newReturnItem), newVariable.copyId)
    }
  }

  private val instance: Rewriter = bottomUp(Rewriter.lift {
    case query @ SingleQuery(clauses) =>
      query.copy(clauses = clauses.flatMap(clauseRewriter))(query.position)(SameId(query.id))
  })
}
