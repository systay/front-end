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

import org.opencypher.v9_0.expressions._
import org.opencypher.v9_0.parser.CypherParser
import org.opencypher.v9_0.util.attribution.{Attributes, SequentialIdGen}
import org.opencypher.v9_0.util.test_helpers.CypherFunSuite

class VariableBindingTest extends CypherFunSuite {


  test("simplest variable binding test") {
    val parser = new CypherParser(Attributes(new SequentialIdGen()))
    val x = parser.parse("MATCH (a) RETURN a.prop")
    val scopes = new Scopes
    val bindings = new VariableBindings

    new TreeWalker(new Scoper(scopes), new VariableBinder(bindings)).visit(x.statement)

    val matchVar = x.statement.findByClass[NodePattern].variable.get
    val expressionVar = x.statement.findByClass[Property].map.asInstanceOf[Variable]

    bindings.get(matchVar.id) should equal(Declaration)
    bindings.get(expressionVar.id) should equal(Reference(matchVar.id))
  }

  test("undeclared variables are bad") {
    val parser = new CypherParser(Attributes(new SequentialIdGen()))
    val x = parser.parse("RETURN a.prop")
    val scopes = new Scopes
    val bindings = new VariableBindings

    intercept[VariableNotDeclaredError](
      new TreeWalker(new Scoper(scopes), new VariableBinder(bindings)).visit(x.statement)
    )
  }
}

