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
import org.opencypher.v9_0.ast.semantics.SemanticState
import org.opencypher.v9_0.expressions.{Expression, Variable}
import org.opencypher.v9_0.util.attribution.{Attributes, SameId}
import org.opencypher.v9_0.util.{Rewriter, bottomUp}

case class expandStar(state: SemanticState, attr: Attributes) extends Rewriter {

  def apply(that: AnyRef): AnyRef = instance(that)

  private val rewriter = Rewriter.lift {
    case clause: With if clause.returnItems.includeExisting =>
      val newReturnItems = returnItems(clause, clause.returnItems.items)
      clause.copy(returnItems = newReturnItems)(clause.position)(SameId(clause.id))

    case clause: PragmaWithout =>
      With(
        distinct = false,
        returnItems = returnItems(clause, Seq.empty, clause.excludedNames),
        orderBy = None, skip = None, limit = None, where = None)(clause.position)(SameId(clause.id))

    case clause: Return if clause.returnItems.includeExisting =>
      val values = clause.returnItems
      val newReturnItems = if (values.includeExisting) returnItems(clause, values.items, clause.excludedNames) else values
      clause.copy(returnItems = newReturnItems, excludedNames = Set.empty)(clause.position)(SameId(clause.id))

    case expandedAstNode =>
      expandedAstNode
  }

  private val instance = bottomUp(rewriter, _.isInstanceOf[Expression])

  private def returnItems(clause: Clause, listedItems: Seq[ReturnItem], excludedNames: Set[String] = Set.empty)
  : ReturnItemsDef = {
    val scope = state.scope(clause).getOrElse {
      throw new IllegalStateException(s"${clause.name} should note its Scope in the SemanticState")
    }

    val clausePos = clause.position
    val symbolNames = scope.symbolNames -- excludedNames
    val expandedItems = symbolNames.toIndexedSeq.sorted.map { id =>
      val idPos = scope.symbolTable(id).definition.position
      // TODO This needs to get scope information from an Attribute later on
      val expr = Variable(id)(idPos)(attr.idGen)
      val alias = expr.copyId
      AliasedReturnItem(expr, alias)(clausePos)(attr.idGen)
    }

    val newItems = expandedItems ++ listedItems
    ReturnItems(includeExisting = false, newItems)(clausePos)(attr.idGen)
  }
}
