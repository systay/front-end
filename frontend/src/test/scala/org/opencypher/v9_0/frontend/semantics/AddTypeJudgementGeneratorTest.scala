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
import org.opencypher.v9_0.expressions.Add
import org.opencypher.v9_0.frontend.semantics.SemanticTestHelper.{mockBinding, mockScoping, mockTypeExpectations}
import org.opencypher.v9_0.frontend.semantics.Types._
import org.opencypher.v9_0.util.ASTNode
import org.opencypher.v9_0.util.test_helpers.CypherFunSuite

object SemanticTestHelper {
  def mockScoping = new Scoping {
    override def scope(ast: ASTNode, incoming: Scope): ScopingResult = ScopingResult(None, None)
  }
  def mockBinding = new VariableBinding {
    override def bind(ast: ASTNode, bindingMode: BindingMode): BindingMode = ReferenceOnly
  }
  def mockTypeExpectations = new TypeExpecting {
    override def visit(ast: ASTNode, bindingMode: BindingMode): Unit = {}
  }
}

class AddTypeJudgementGeneratorTest extends CypherFunSuite with AstConstructionTestSupport {

  // Infix specializations:
  // "a" + "b" => "ab"
  // "a" + 1 => "a1"
  // "a" + 1.1 => "a1.1"
  // 1 + "b" => "1b"
  // 1 + 1 => 2
  // 1 + 1.1 => 2.1
  // 1.1 + "b" => "1.1b"
  // 1.1 + 1 => 2.1
  // 1.1 + 1.1 => 2.2
  // [a] + [b] => [a, b]
  // [a] + b => [a, b]
  // a + [b] => [a, b]


  implicit def toSet(t: NewCypherType): Set[NewCypherType] = Set(t)


  def testValidTypes(lhs: Set[NewCypherType], rhs: Set[NewCypherType])(expectedResult: Set[NewCypherType]): Unit = test(s"${lhs.mkString("(", ", ", ")")} + ${rhs.mkString("(", ", ", ")")} -> ${expectedResult.mkString("(", ", ", ")")}") {
    val types = new TypeJudgements
    val aVariable = varFor("a")
    val bVariable = varFor("b")
    val ast = Add(aVariable, bVariable)(pos)
    val bindings = new VariableBindings
    val expectations = new TypeExpectations
    val bindingsLookup = new BindingsLookup(ast, bindings)
    val typer = new TypeJudgementGenerator(types, bindingsLookup, expectations)

    bindings.set(aVariable.id, Declaration)
    bindings.set(bVariable.id, Declaration)

    expectations.set(aVariable.id, new TypeInfo(lhs, true))
    expectations.set(bVariable.id, new TypeInfo(rhs, true))

    new TreeWalker(mockScoping, mockBinding, mockTypeExpectations, typer).visit(ast)

    types.get(ast.id) should equal(new TypeInfo(expectedResult, true))
  }

  // shouldHandleAllSpecializations
    testValidTypes(StringT, StringT)(StringT)
    testValidTypes(StringT, IntegerT)(StringT)
    testValidTypes(StringT, FloatT)(StringT)
    testValidTypes(IntegerT, StringT)(StringT)
    testValidTypes(IntegerT, IntegerT)(IntegerT)
    testValidTypes(IntegerT, FloatT)(FloatT)
    testValidTypes(FloatT, StringT)(StringT)
    testValidTypes(FloatT, IntegerT)(FloatT)
    testValidTypes(FloatT, FloatT)(FloatT)
    testValidTypes(DurationT, DurationT)(DurationT)
    testValidTypes(DateT, DurationT)(DateT)
    testValidTypes(DurationT, DateT)(DateT)
    testValidTypes(TimeT, DurationT)(TimeT)
    testValidTypes(DurationT, TimeT)(TimeT)
    testValidTypes(LocalTimeT, DurationT)(LocalTimeT)
    testValidTypes(DurationT, LocalTimeT)(LocalTimeT)
    testValidTypes(DateTimeT, DurationT)(DateTimeT)
    testValidTypes(DurationT, DateTimeT)(DateTimeT)
    testValidTypes(LocalDateT, DurationT)(LocalDateT)
    testValidTypes(DurationT, LocalDateT)(LocalDateT)

    testValidTypes(ListT(NodeT), ListT(NodeT))(ListT(NodeT))
    testValidTypes(ListT(FloatT), ListT(FloatT))(ListT(FloatT))

    testValidTypes(ListT(NodeT), NodeT)(ListT(NodeT))
    testValidTypes(ListT(FloatT), FloatT)(ListT(FloatT))

    testValidTypes(NodeT, ListT(NodeT))(ListT(NodeT))
    testValidTypes(FloatT, ListT(FloatT))(ListT(FloatT))

    testValidTypes(ListT(?), ListT(?))(ListT(?))


  // shouldHandleCombinedSpecializations
    testValidTypes(FloatT | StringT, IntegerT)(FloatT | StringT)
    testValidTypes(FloatT | ListT(FloatT), FloatT)(FloatT | ListT(FloatT))
    testValidTypes(FloatT, FloatT | ListT(FloatT))(FloatT | ListT(FloatT))

  // shouldHandleCoercions
    testValidTypes(ListT(FloatT), IntegerT)(ListT(FloatT, IntegerT))
    testValidTypes(FloatT | ListT(FloatT), IntegerT)(FloatT | ListT(FloatT, IntegerT))

  // should concatenate different typed lists
    testValidTypes(ListT(IntegerT), ListT(StringT))(ListT(IntegerT, StringT))

  // should concatenate vector element of other type after list
    testValidTypes(IntegerT, ListT(StringT))(ListT(IntegerT, StringT))

  // should concatenate vector element of other type before list
    testValidTypes(ListT(IntegerT), StringT)(ListT(IntegerT, StringT))

  // should concatenate same typed lists
    testValidTypes(ListT(IntegerT), ListT(IntegerT))(ListT(IntegerT))

  // should concatenate nested lists
    testValidTypes(
      ListT(ListT(IntegerT)),
      ListT(ListT(IntegerT)))(
      ListT(ListT(IntegerT)))
    testValidTypes(
      ListT(ListT(IntegerT)),
      ListT(IntegerT))(
      ListT(ListT(IntegerT), IntegerT))
    testValidTypes(
      ListT(ListT(IntegerT)),
      IntegerT)(
      ListT(ListT(IntegerT), IntegerT))

  // should work with ORed types
    testValidTypes(
      IntegerT | ListT(StringT),
      ListT(StringT) | IntegerT)(
      ListT(StringT) | ListT(StringT, IntegerT) | IntegerT)

    testValidTypes(
      IntegerT | ListT(IntegerT),
      StringT)(
      StringT | ListT(IntegerT, StringT))

    testValidTypes(
      IntegerT | ListT(IntegerT),
      BoolT)(
      ListT(IntegerT, BoolT))
}

