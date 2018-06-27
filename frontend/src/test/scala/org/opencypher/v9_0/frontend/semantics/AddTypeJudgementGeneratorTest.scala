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
    testValidTypes(StringType, StringType)(StringType)
    testValidTypes(StringType, IntegerType)(StringType)
    testValidTypes(StringType, FloatType)(StringType)
    testValidTypes(IntegerType, StringType)(StringType)
    testValidTypes(IntegerType, IntegerType)(IntegerType)
    testValidTypes(IntegerType, FloatType)(FloatType)
    testValidTypes(FloatType, StringType)(StringType)
    testValidTypes(FloatType, IntegerType)(FloatType)
    testValidTypes(FloatType, FloatType)(FloatType)
    testValidTypes(DurationType, DurationType)(DurationType)
    testValidTypes(DateType, DurationType)(DateType)
    testValidTypes(DurationType, DateType)(DateType)
    testValidTypes(TimeType, DurationType)(TimeType)
    testValidTypes(DurationType, TimeType)(TimeType)
    testValidTypes(LocalTimeType, DurationType)(LocalTimeType)
    testValidTypes(DurationType, LocalTimeType)(LocalTimeType)
    testValidTypes(DateTimeType, DurationType)(DateTimeType)
    testValidTypes(DurationType, DateTimeType)(DateTimeType)
    testValidTypes(LocalDateTimeType, DurationType)(LocalDateTimeType)
    testValidTypes(DurationType, LocalDateTimeType)(LocalDateTimeType)

    testValidTypes(ListType(NodeType), ListType(NodeType))(ListType(NodeType))
    testValidTypes(ListType(FloatType), ListType(FloatType))(ListType(FloatType))

    testValidTypes(ListType(NodeType), NodeType)(ListType(NodeType))
    testValidTypes(ListType(FloatType), FloatType)(ListType(FloatType))

    testValidTypes(NodeType, ListType(NodeType))(ListType(NodeType))
    testValidTypes(FloatType, ListType(FloatType))(ListType(FloatType))

    testValidTypes(ListType(?), ListType(?))(ListType(?))


  // shouldHandleCombinedSpecializations
    testValidTypes(FloatType | StringType, IntegerType)(FloatType | StringType)
    testValidTypes(FloatType | ListType(FloatType), FloatType)(FloatType | ListType(FloatType))
    testValidTypes(FloatType, FloatType | ListType(FloatType))(FloatType | ListType(FloatType))

  // shouldHandleCoercions
    testValidTypes(ListType(FloatType), IntegerType)(ListType(FloatType, IntegerType))
    testValidTypes(FloatType | ListType(FloatType), IntegerType)(FloatType | ListType(FloatType, IntegerType))

  // should concatenate different typed lists
    testValidTypes(ListType(IntegerType), ListType(StringType))(ListType(IntegerType, StringType))

  // should concatenate vector element of other type after list
    testValidTypes(IntegerType, ListType(StringType))(ListType(IntegerType, StringType))

  // should concatenate vector element of other type before list
    testValidTypes(ListType(IntegerType), StringType)(ListType(IntegerType, StringType))

  // should concatenate same typed lists
    testValidTypes(ListType(IntegerType), ListType(IntegerType))(ListType(IntegerType))

  // should concatenate nested lists
    testValidTypes(
      ListType(ListType(IntegerType)),
      ListType(ListType(IntegerType)))(
      ListType(ListType(IntegerType)))
    testValidTypes(
      ListType(ListType(IntegerType)),
      ListType(IntegerType))(
      ListType(ListType(IntegerType), IntegerType))
    testValidTypes(
      ListType(ListType(IntegerType)),
      IntegerType)(
      ListType(ListType(IntegerType), IntegerType))

  // should work with ORed types
    testValidTypes(
      IntegerType | ListType(StringType),
      ListType(StringType) | IntegerType)(
      ListType(StringType) | ListType(StringType, IntegerType) | IntegerType)

    testValidTypes(
      IntegerType | ListType(IntegerType),
      StringType)(
      StringType | ListType(IntegerType, StringType))

    testValidTypes(
      IntegerType | ListType(IntegerType),
      BoolType)(
      ListType(IntegerType, BoolType))
}

