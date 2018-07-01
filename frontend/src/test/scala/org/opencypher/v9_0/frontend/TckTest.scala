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
package org.opencypher.v9_0.frontend

import org.opencypher.tools.tck.api._
import org.opencypher.v9_0.frontend.semantics._
import org.opencypher.v9_0.parser.CypherParser
import org.opencypher.v9_0.util.attribution.SequentialIdGen
import org.scalatest.FunSpec

class TckTest extends FunSpec {

  val scenariosPerFeature: Map[String, Seq[Scenario]] =
    CypherTCK.allTckScenarios.foldLeft(Map.empty[String, Seq[Scenario]]) {
      case (acc, scenario: Scenario) =>
        val soFar: Seq[Scenario] = acc.getOrElse(scenario.featureName, Seq.empty[Scenario])
        acc + (scenario.featureName -> (soFar :+ scenario))
    }
  var x = 0

  scenariosPerFeature foreach {
    case (featureName, scenarios) =>
      describe(featureName) {
        scenarios.foreach {
          scenarioObj =>
            describe(scenarioObj.name) {
              scenarioObj.steps foreach {
                case Execute(query, _, _) =>
                  x = x + 1
                  it(query + x) {
                    testQuery(query)
                  }
                case _ =>
              }
            }
        }
      }
  }

  private def testQuery(query: String): Unit = {
    val x = CypherParser.parse(query, new SequentialIdGen())
    val judgements = new TypeJudgements
    val expectations = new TypeExpectations
    val expector = new TypeExpectationsGenerator(expectations, judgements)
    val expector2 = new TypeExpectationsAfterJudgements(expectations, judgements)
    val variableBindings = new VariableBindings
    val bindingsLookup = new BindingsLookup(x.statement, variableBindings)
    val typer = new TypeJudgementGenerator(judgements, bindingsLookup, expectations)
    val typeChecker = new TypeChecker(expectations, judgements)
    val scopes = new Scopes
    val scoping = new Scoper(scopes)
    val binder = new VariableBinder(variableBindings, scopes)
    val upVisitor = expector2 andThen typer andThen typeChecker
    val walker = new TreeWalker(scoping, binder, expector, upVisitor)
    walker.visit(x.statement)
  }
}