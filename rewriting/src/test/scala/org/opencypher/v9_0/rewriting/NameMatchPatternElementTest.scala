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
package org.opencypher.v9_0.rewriting

import org.opencypher.v9_0.ast.SequentialIds
import org.opencypher.v9_0.rewriting.rewriters.{nameMatchPatternElements, nameUpdatingClauses}
import org.opencypher.v9_0.util.attribution.Attributes
import org.opencypher.v9_0.util.test_helpers.CypherFunSuite


class NameMatchPatternElementTest extends CypherFunSuite with SequentialIds {

  import org.opencypher.v9_0.parser.ParserFixture.parse

  val attributes = Attributes(idGen)

  test("name all NodePatterns in Query") {
    val original = parse("MATCH (n)-[r:Foo]->() RETURN n")
    val expected = parse("MATCH (n)-[r:Foo]->(`  UNNAMED20`) RETURN n")

    val result = original.rewrite(nameMatchPatternElements(attributes))
    assert(result === expected)
  }

  test("name all RelationshipPatterns in Query") {
    val original = parse("MATCH (n)-[:Foo]->(m) WHERE (n)-[:Bar]->(m) RETURN n")
    val expected = parse("MATCH (n)-[`  UNNAMED10`:Foo]->(m) WHERE (n)-[:Bar]->(m) RETURN n")

    val result = original.rewrite(nameMatchPatternElements(attributes))
    assert(result === expected)
  }

  test("rename unnamed varlength paths") {
    val original = parse("MATCH (n)-[:Foo*]->(m) RETURN n")
    val expected = parse("MATCH (n)-[`  UNNAMED10`:Foo*]->(m) RETURN n")

    val result = original.rewrite(nameMatchPatternElements(attributes))
    assert(result === expected)
  }

  test("match (a) create unique (a)-[:X]->() return a") {
    val original = parse("match (a) create unique p=(a)-[:X]->() return p")
    val expected = parse("match (a) create unique p=(a)-[`  UNNAMED30`:X]->(`  UNNAMED37`) return p")

    val result = original.rewrite(nameUpdatingClauses(attributes))
    assert(result === expected)
  }

  test("match (a) create (a)-[:X]->() return a") {
    val original = parse("match (a) create (a)-[:X]->() return a")
    val expected = parse("match (a) create (a)-[`  UNNAMED21`:X]->(`  UNNAMED28`) return a")

    val result = original.rewrite(nameUpdatingClauses(attributes))
    assert(result === expected)
  }

  test("merge (a) merge p = (a)-[:R]->() return p") {
    val original = parse("merge (a) merge p = (a)-[:R]->() return p")
    val expected = parse("merge (a) merge p = (a)-[`  UNNAMED24`:R]->(`  UNNAMED31`) return p")

    val result = original.rewrite(nameUpdatingClauses(attributes))
    assert(result === expected)
  }

  test("merge (a)-[:R]->() return a") {
    val original = parse("merge (a)-[:R]->() return a")
    val expected = parse("merge (a)-[`  UNNAMED10`:R]->(`  UNNAMED17`) return a")

    val result = original.rewrite(nameUpdatingClauses(attributes))
    assert(result === expected)
  }

  test("does not touch parameters") {
    val original = parse("MATCH (n)-[r:Foo]->({p}) RETURN n")
    val expected = parse("MATCH (n)-[r:Foo]->(`  UNNAMED20` {p}) RETURN n")

    val result = original.rewrite(nameMatchPatternElements(attributes))
    assert(result === expected)
  }

  test("names all unnamed var length relationships") {
    val original = parse("MATCH (a:Artist)-[:WORKED_WITH* { year: 1988 }]->(b:Artist) RETURN *")
    val expected = parse("MATCH (a:Artist)-[`  UNNAMED17`:WORKED_WITH* { year: 1988 }]->(b:Artist) RETURN *")

    val result = original.rewrite(nameMatchPatternElements(attributes))
    assert(result === expected)
  }
}
