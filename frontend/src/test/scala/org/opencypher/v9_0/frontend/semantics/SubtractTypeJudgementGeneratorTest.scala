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
import org.opencypher.v9_0.frontend.semantics.Types._
import org.opencypher.v9_0.util.test_helpers.CypherFunSuite

class SubtractTypeJudgementGeneratorTest extends CypherFunSuite with AstConstructionTestSupport with BinaryOpSemanticTest {

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

  override def op = Subtract.apply
}

