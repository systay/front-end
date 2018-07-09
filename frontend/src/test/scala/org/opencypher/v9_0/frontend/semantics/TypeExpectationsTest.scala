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

import org.opencypher.v9_0.ast.Statement
import org.opencypher.v9_0.expressions._
import org.opencypher.v9_0.frontend.semantics.Types._
import org.opencypher.v9_0.parser.CypherParser
import org.opencypher.v9_0.util.attribution.SequentialIdGen
import org.opencypher.v9_0.util.test_helpers.CypherFunSuite

class TypeExpectationsTest extends CypherFunSuite {

  def parseAndAnalyse(q: String): (TypeExpectations, Statement) = {
    val x = CypherParser.parse(q, new SequentialIdGen())
    val scopes = new Scopes
    val bindings = new VariableBindings
    val expectations = new TypeExpectations
    val typer = new TypeExpectationsGenerator(expectations, new TypeJudgements)
    new TreeWalker(new Scoper(scopes), new VariableBinder(bindings, scopes), typer, mock[BottomUpVisitor]).visit(x.statement)
    (expectations, x.statement)
  }

  test("MATCH expects nodes") {
    val (expectations, statement) = parseAndAnalyse("MATCH (a) RETURN a")

    val declarationId = statement.findByClass[NodePattern].variable.get.id
    expectations.get(declarationId) should equal(NonNullableType(NodeT))
  }

  test("Nodes expect maps as properties") {
    val (expectations, statement) = parseAndAnalyse("MATCH (a {id: 42}) RETURN a")

    val map = statement.findByClass[MapExpression].id
    expectations.get(map) should equal(NonNullableType(MapT.MapOfUnknown))
  }

  test("Relationships expect maps as properties") {
    val (expectations, statement) = parseAndAnalyse("MATCH ()-[r {id: 42}]-() RETURN r")

    val map = statement.findByClass[MapExpression].id
    expectations.get(map) should equal(NonNullableType(MapT.MapOfUnknown))
  }

  test("WHERE expected predicates") {
    val (expectations, statement) = parseAndAnalyse("MATCH (a) WHERE a.prop RETURN a")

    val property = statement.findByClass[Property]
    val propId = property.id
    expectations.get(propId) should equal(NullableType(BoolT))
    expectations.get(property.map.id) should equal(NullableType(NodeT, RelationshipT, TimeT, DateT, MapT.MapOfUnknown))
  }

  test("UNWIND expects lists") {
    val (expectations, statement) = parseAndAnalyse("UNWIND [1,2,3] as X RETURN X")

    val property = statement.findByClass[ListLiteral]
    expectations.get(property.id) should equal(NullableType(ListT.ListOfUnknown))
  }

}

