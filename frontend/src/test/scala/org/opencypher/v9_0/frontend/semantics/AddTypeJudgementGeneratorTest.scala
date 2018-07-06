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
import org.opencypher.v9_0.frontend.semantics.Types._
import org.opencypher.v9_0.util.test_helpers.CypherFunSuite

class AddTypeJudgementGeneratorTest extends CypherFunSuite with AstConstructionTestSupport with BinaryOpSemanticTest {

  override def op = Add.apply

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

  // should handle concatenate, prepending and appending to lists
  testValidTypes(ListT(FloatT), IntegerT)(ListT(FloatT, IntegerT))
  testValidTypes(FloatT | ListT(FloatT), IntegerT)(FloatT | ListT(FloatT, IntegerT))
  testValidTypes(ListT(IntegerT), ListT(StringT))(ListT(IntegerT, StringT))
  testValidTypes(IntegerT, ListT(StringT))(ListT(IntegerT, StringT))
  testValidTypes(ListT(IntegerT), StringT)(ListT(IntegerT, StringT))
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
