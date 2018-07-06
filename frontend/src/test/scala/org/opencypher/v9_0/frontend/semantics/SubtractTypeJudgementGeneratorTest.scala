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
import org.opencypher.v9_0.expressions.Subtract
import org.opencypher.v9_0.frontend.semantics.SemanticTestHelper.{mockBinding, mockScoping, mockTypeExpectations}
import org.opencypher.v9_0.frontend.semantics.Types._
import org.opencypher.v9_0.util.test_helpers.CypherFunSuite

class SubtractTypeJudgementGeneratorTest extends CypherFunSuite with AstConstructionTestSupport {

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
    val ast = Subtract(aVariable, bVariable)(pos)
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
  testValidTypes(IntegerT, IntegerT)(IntegerT)
  testValidTypes(IntegerT, FloatT)(FloatT)
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
}

