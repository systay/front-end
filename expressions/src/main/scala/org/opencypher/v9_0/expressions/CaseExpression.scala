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
package org.opencypher.v9_0.expressions

import org.opencypher.v9_0.util.InputPosition
import org.opencypher.v9_0.util.attribution.IdGen

case class CaseExpression(
                           expression: Option[Expression],
                           alternatives: IndexedSeq[(Expression, Expression)],
                           default: Option[Expression]
                         )(val position: InputPosition)(implicit override val idGen: IdGen) extends Expression {

  lazy val possibleExpressions: IndexedSeq[Expression] = alternatives.map(_._2) ++ default

}

object CaseExpression {
  def apply(
             expression: Option[Expression],
             alternatives: List[(Expression, Expression)],
             default: Option[Expression]
           )(position: InputPosition)(implicit idGen: IdGen) :CaseExpression =
    CaseExpression(expression, alternatives.toIndexedSeq, default)(position)
}
