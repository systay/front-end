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

import org.opencypher.v9_0.frontend.semantics.Types.{IntegerT, ListT, MapT, StringT}
import org.opencypher.v9_0.util.test_helpers.CypherFunSuite

class TypesTest extends CypherFunSuite {

  test("List(A) is satisfied by List(A)") {
    val expectation = new TypeInfo(Set(ListT(IntegerT, StringT)), false)
    val judgment = new TypeInfo(Set(ListT(IntegerT, StringT)), false)
    (expectation isSatisfiedBy judgment) should equal(true)
  }

  test("List(List((A,B) is satisfied by List(List(A))") {
    val expectation = new TypeInfo(Set(ListT(ListT(IntegerT, StringT))), false)
    val judgment = new TypeInfo(Set(ListT(ListT(IntegerT))), false)
    (expectation isSatisfiedBy judgment) should equal(true)
  }

  test("List(A) is not satisfied by List(B)") {
    val expectation = new TypeInfo(Set(ListT(IntegerT)), false)
    val judgment = new TypeInfo(Set(ListT(StringT)), false)
    (expectation isSatisfiedBy judgment) should equal(false)
  }

  test("List(?) is satisfied by List(B)") {
    val expectation = new TypeInfo(Set(ListT.ListOfUnknown), false)
    val judgment = new TypeInfo(Set(ListT(StringT)), false)
    (expectation isSatisfiedBy judgment) should equal(true)
  }

  test("List(A,B) is satisfied by List(A)") {
    val expectation = new TypeInfo(Set(ListT(IntegerT, StringT)), false)
    val judgement = new TypeInfo(Set(ListT(IntegerT)), false)
    (expectation isSatisfiedBy judgement) should equal(true)
  }

  test("(A) is satisfied by (A)") {
    val expectation = new TypeInfo(Set(StringT), false)
    val judgement = new TypeInfo(Set(StringT), false)
    (expectation isSatisfiedBy judgement) should equal(true)
  }

  test("(B) is not satisfied by (A)") {
    val expectation = new TypeInfo(Set(IntegerT), false)
    val judgement = new TypeInfo(Set(StringT), false)
    (expectation isSatisfiedBy judgement) should equal(false)
  }

  test("(A) is satisfied by (A, B)") {
    val expectation = new TypeInfo(Set(StringT), false)
    val judgement = new TypeInfo(Set(StringT, IntegerT), false)
    (expectation isSatisfiedBy judgement) should equal(true)
  }

  test("(A, B) is satisfied by (A)") {
    val expectation = new TypeInfo(Set(StringT, IntegerT), false)
    val judgement = new TypeInfo(Set(StringT), false)
    (expectation isSatisfiedBy judgement) should equal(true)
  }

  test("Map(A) is satisfied by Map(A)") {
    val expectation = new TypeInfo(Set(MapT(IntegerT, StringT)), false)
    val judgment = new TypeInfo(Set(MapT(IntegerT, StringT)), false)
    (expectation isSatisfiedBy judgment) should equal(true)
  }

  test("Map((A,B) is satisfied by Map(A))") {
    val expectation = new TypeInfo(Set(MapT(MapT(IntegerT, StringT))), false)
    val judgment = new TypeInfo(Set(MapT(MapT(IntegerT))), false)
    (expectation isSatisfiedBy judgment) should equal(true)
  }

  test("Map(A) is not satisfied by Map(B)") {
    val expectation = new TypeInfo(Set(MapT(IntegerT)), false)
    val judgment = new TypeInfo(Set(MapT(StringT)), false)
    (expectation isSatisfiedBy judgment) should equal(false)
  }

  test("Map(?) is satisfied by Map(B)") {
    val expectation = new TypeInfo(Set(MapT.MapOfUnknown), false)
    val judgment = new TypeInfo(Set(MapT(StringT)), false)
    (expectation isSatisfiedBy judgment) should equal(true)
  }

  test("Map(A,B) is satisfied by Map(A)") {
    val expectation = new TypeInfo(Set(MapT(IntegerT, StringT)), false)
    val judgement = new TypeInfo(Set(MapT(IntegerT)), false)
    (expectation isSatisfiedBy judgement) should equal(true)
  }

}
