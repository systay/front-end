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

import org.opencypher.v9_0.ast.{Clause, Match, Merge, Where}
import org.opencypher.v9_0.expressions
import org.opencypher.v9_0.expressions.{Expression, _}
import org.opencypher.v9_0.util._
import org.opencypher.v9_0.util.attribution.{Attributes, SameId}

case class addUniquenessPredicates(attributes: Attributes) extends Rewriter {

  def apply(that: AnyRef): AnyRef = instance(that)

  private val rewriter = Rewriter.lift {
    case m: Match =>
      val uniqueRels: Seq[UniqueRel] = collectUniqueRels(m.pattern)
      if (uniqueRels.size < 2) {
        m
      } else {
        val newWhere = addPredicate(m, uniqueRels, m.where)
        m.copy(where = newWhere)(m.position)(SameId(m.id))
      }
    case m: Merge =>
      val uniqueRels: Seq[UniqueRel] = collectUniqueRels(m.pattern)
      if (uniqueRels.size < 2) {
        m
      } else {
        val newWhere = addPredicate(m, uniqueRels, m.where)
        m.copy(where = newWhere)(m.position)(SameId(m.id))
      }
  }

  private def addPredicate(clause: Clause, uniqueRels:  Seq[UniqueRel], where: Option[Where]): Option[Where] = {
    val maybePredicate: Option[Expression] = createPredicateFor(uniqueRels, clause)
    val newWhere: Option[Where] = (where, maybePredicate) match {
      case (Some(oldWhere), Some(newPredicate)) =>
        val and = And(oldWhere.expression, newPredicate)(clause.position)(attributes.copy(clause.id))
        Some(oldWhere.copy(expression = and)(clause.position)(attributes.copy(clause.id)))

      case (None, Some(newPredicate)) =>
        Some(Where(expression = newPredicate)(clause.position)(attributes.copy(clause.id)))

      case (oldWhere, None) => oldWhere
    }
    newWhere
  }

  private val instance = bottomUp(rewriter, _.isInstanceOf[Expression])

  def collectUniqueRels(pattern: ASTNode): Seq[UniqueRel] =
    pattern.treeFold(Seq.empty[UniqueRel]) {
      case _: ShortestPaths =>
        acc => (acc, None)

      case RelationshipChain(_, patRel@RelationshipPattern(optIdent, types, _, _, _, _, _), _) =>
        acc => {
          val ident = optIdent.getOrElse(throw new InternalException("This rewriter cannot work with unnamed patterns"))
          (acc :+ UniqueRel(ident, types.toSet, patRel.isSingleLength), Some(identity))
        }
    }

  private def createPredicateFor(uniqueRels: Seq[UniqueRel], clause: Clause): Option[Expression] = {
    createPredicatesFor(uniqueRels, clause).reduceOption(expressions.And(_, _)(clause.position)(attributes.copy(clause.id)))
  }

  def createPredicatesFor(uniqueRels: Seq[UniqueRel], astNode: ASTNode): Seq[Expression] =
    for {
      x <- uniqueRels
      y <- uniqueRels if x.name < y.name && !x.isAlwaysDifferentFrom(y)
    } yield {
      val equals = Equals(x.variable.copyId, y.variable.copyId)(astNode.position)(attributes.copy(astNode.id))

      (x.singleLength, y.singleLength) match {
        case (true, true) =>
          Not(equals)(astNode.position)(attributes.copy(astNode.id))

        case (true, false) =>
          NoneIterablePredicate(y.variable.copyId, y.variable.copyId, Some(equals))(astNode.position)(attributes.copy(astNode.id))

        case (false, true) =>
          NoneIterablePredicate(x.variable.copyId, x.variable.copyId, Some(equals))(astNode.position)(attributes.copy(astNode.id))

        case (false, false) =>
          val anyIterablePredicate = AnyIterablePredicate(y.variable.copyId, y.variable.copyId, Some(equals))(astNode.position)(attributes.copy(astNode.id))
          NoneIterablePredicate(x.variable.copyId, x.variable.copyId, Some(anyIterablePredicate))(astNode.position)(attributes.copy(astNode.id))
      }
    }

  case class UniqueRel(variable: LogicalVariable, types: Set[RelTypeName], singleLength: Boolean) {
    def name = variable.name

    def isAlwaysDifferentFrom(other: UniqueRel) =
      types.nonEmpty && other.types.nonEmpty && (types intersect other.types).isEmpty
  }
}
