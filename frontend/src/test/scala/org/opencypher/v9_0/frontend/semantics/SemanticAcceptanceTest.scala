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
import org.opencypher.v9_0.parser.{CypherParser, ParseResult}
import org.opencypher.v9_0.util.attribution.SequentialIdGen
import org.opencypher.v9_0.util.test_helpers.CypherFunSuite

// This class tests all of semantic analysis and makes it possible to assert on any state
class SemanticAcceptanceTest extends CypherFunSuite {
  def testThis(q: String): (Statement, Scopes, VariableBindings, TypeExpectations, TypeJudgements) =  {
    val ParseResult(statement, _) = CypherParser.parse(q, new SequentialIdGen())

    // State
    val judgements = new TypeJudgements
    val expectations = new TypeExpectations
    val bindings = new VariableBindings
    val scopes = new Scopes
    val bindingsLookup = new BindingsLookup(statement, bindings)

    // Visitors
    val expector = new TypeExpectationsGenerator(expectations, judgements)
    val typeExpectationsAfterChildrenAreTyped = new TypeExpectationsAfterJudgements(expectations, judgements)
    val typeJudgements = new TypeJudgementGenerator(judgements, bindingsLookup, expectations)
    val typeChecker = new TypeChecker(expectations, judgements)
    val scoping = new Scoper(scopes)
    val binder = new VariableBinder(bindings, scopes)
    val upVisitor: BottomUpVisitor = typeExpectationsAfterChildrenAreTyped andThen typeJudgements andThen typeChecker

    // Assembler
    val walker = new TreeWalker(scoping, binder, expector, upVisitor)

    walker.visit(statement)

    (statement, scopes, bindings, expectations, judgements)
  }

  test("CREATE ({foo: 'bar'})") {
    val (statement, scopes, bindings, expectations, judgements) =
      testThis("CREATE ({foo: 'bar'})")
  }

  test("MERGE (a) ON CREATE SET a:FOO RETURN a") {
    val (statement, scopes, bindings, expectations, judgements) =
      testThis("MERGE (a) ON CREATE SET a:FOO RETURN a")
  }

  test("CREATE ()-[:T {id: 42}]->()") {
    val (x) = testThis("CREATE ()-[:T {id: 42}]->()")
    println(x)
  }
}
