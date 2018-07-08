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
package org.opencypher.v9_0.frontend.semantics

import org.opencypher.v9_0.expressions.Expression
import org.opencypher.v9_0.util.ASTNode
import org.opencypher.v9_0.util.attribution.Attribute

/**
  * This class checks whether type expectations and judgements align well enough to so that executing the query makes sense.
  * Type errors can still occur, but that would be runtime checks and not something we can detect at this stage.
  */
class TypeChecker(typeExpectations: TypeExpectations, typeJudgements: TypeJudgements) extends BottomUpVisitor {
  override def visit(node: ASTNode): Unit = node match {
    case e: Expression =>
      val expected: TypeInfo = getTypeOf(e, typeExpectations)
      val judged: TypeInfo = getTypeOf(e, typeJudgements)

      if (!(expected isSatisfiedBy judged)) {
        // Uh-oh... TODO: Let's check if coercions are possible
        throw new TypeExpectationsNotMetException(expected, judged, e)
      }

    case _ =>
  }

  private def getTypeOf(e: Expression, typeAttribute: Attribute[TypeInfo]): TypeInfo = try typeAttribute.get(e.id)
  catch {
    case t: IndexOutOfBoundsException =>
      throw new RuntimeException(s"Type judgement or expectation was missing for: $e", t)
    case t: Exception =>
      throw new RuntimeException(s"Something went wrong on the way to heaven: \n$e", t)
  }

}
