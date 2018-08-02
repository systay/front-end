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

import org.opencypher.v9_0.ast.AstConstructionTestSupport
import org.opencypher.v9_0.expressions.Expression
import org.opencypher.v9_0.frontend.semantics.SemanticTestHelper.{mockBinding, mockScoping, mockTypeExpectations}
import org.opencypher.v9_0.frontend.semantics.Types.NewCypherType
import org.opencypher.v9_0.util.test_helpers.CypherFunSuite
import org.opencypher.v9_0.util.{ASTNode, InputPosition}

object SemanticTestHelper {
  def mockScoping: Scoping = new Scoping {
    override def scope(ast: ASTNode, incoming: Scope): ScopingResult = ScopingResult(None, None)
  }
  def mockBinding: VariableBinding = new VariableBinding {
    override def bind(ast: ASTNode, bindingMode: VariableContext): VariableContext = ReferenceOnly
  }
  def mockTypeExpectations: TypeExpecting = new TypeExpecting {
    override def visit(ast: ASTNode, bindingMode: VariableContext): Unit = {}
  }
}

trait BinaryOpSemanticTest {
  self: CypherFunSuite with AstConstructionTestSupport =>

  implicit def toSet(t: NewCypherType): Set[NewCypherType] = Set(t)

  def op: (Expression, Expression) => InputPosition => Expression

  def testValidTypes(lhs: Set[NewCypherType], rhs: Set[NewCypherType])(expectedResult: Set[NewCypherType]): Unit =
    test(s"${lhs.mkString("(", ", ", ")")} + ${rhs.mkString("(", ", ", ")")} -> ${expectedResult.mkString("(", ", ", ")")}") {
      for {
        l <- Seq(false, true)
        r <- Seq(false, true)
      } {
        testWithNullableSpecified(lhs, rhs, expectedResult, l, r)
      }
    }

  private def testWithNullableSpecified(lhs: Set[NewCypherType],
                                        rhs: Set[NewCypherType],
                                        expectedResult: Set[NewCypherType],
                                        lhsNullable: Boolean,
                                        rhsNullable: Boolean): Unit = {
    val types = new TypeJudgements
    val aVariable = varFor("a")
    val bVariable = varFor("b")
    val ast: Expression = op(aVariable, bVariable)(pos)
    val bindings = new VariableBindings
    val expectations = new TypeExpectations
    val bindingsLookup = new BindingsLookup(ast, bindings)
    val typer = new TypeJudgementGenerator(types, bindingsLookup, expectations)

    bindings.set(aVariable.id, Declaration)
    bindings.set(bVariable.id, Declaration)
    expectations.set(aVariable.id, new TypeInfo(lhs, lhsNullable))
    expectations.set(bVariable.id, new TypeInfo(rhs, rhsNullable))
    new TreeWalker(mockScoping, mockBinding, mockTypeExpectations, typer).visit(ast)
    types.get(ast.id) should equal(new TypeInfo(expectedResult, lhsNullable || rhsNullable))
  }
}
