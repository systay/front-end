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

import org.opencypher.v9_0.ast._
import org.opencypher.v9_0.expressions._
import org.opencypher.v9_0.parser.CypherParser
import org.opencypher.v9_0.util.attribution.{Attributes, SequentialIdGen}
import org.opencypher.v9_0.util.test_helpers.CypherFunSuite

import scala.reflect.ClassTag

class VariableBindingTest extends CypherFunSuite {


  private def parseAndAnalyse(q: String) = {
    val scopes = new Scopes
    val bindings = new VariableBindings
    val binder = new VariableBinder(bindings)
    val parser = new CypherParser(Attributes(new SequentialIdGen()))
    val x =  parser.parse(q)


    new TreeWalker(new Scoper(scopes), binder, mock[TypeExpecting]).visit(x.statement)
    (bindings, x.statement)
  }
  
  test("undeclared variables are bad") {
    intercept[VariableNotDeclaredError](
      parseAndAnalyse("RETURN a.prop")
    )
  }

  testa[Unwind]("UNWIND [1,2,3] as x RETURN x",                     (unwind: Unwind) => unwind.variable)
  testa[Match]("MATCH (x) RETURN x",                                (m: Match) => m.findByClass[NodePattern].variable.get)
  testa[Match]("MATCH ()-[r]-() RETURN r",                          (m: Match) => m.findByClass[RelationshipPattern].variable.get)
  testa[Merge]("MERGE (x) RETURN x",                                (m: Merge) => m.findByClass[NodePattern].variable.get)
  testa[LoadCSV]("LOAD CSV FROM 'http://url/' AS line RETURN line", (m: LoadCSV) => m.variable)
  testa[Create]("CREATE (x) RETURN x",                              (m: Create) => m.findByClass[NodePattern].variable.get)


  test("FOREACH") {
    val (bindings, statement) =  parseAndAnalyse("MATCH (a) FOREACH(x in [1,2,3] | SET a.prop = x)")

    val declarationId = statement.findByClass[Foreach].variable.id
    val referenceId = statement.findByClass[SetPropertyItem].expression.id

    bindings.get(declarationId) should equal(Declaration)
    bindings.get(referenceId) should equal(Reference(declarationId))
  }

  test("CREATE should not accept an already bound variable") {
    intercept[VariableAlreadyDeclaredInScopeException](
      parseAndAnalyse("MATCH (a) CREATE (a)")
    )
  }

  test("CREATE should accept an already bound variable when creating relationships") {
    val (bindings, statement) =  parseAndAnalyse("MATCH (a) CREATE (a)-[:T]->(a)")

    val declarationId = statement.findByClass[Match].findByClass[NodePattern].variable.get.id
    val referenceId = statement.findByClass[Create].findByClass[NodePattern].variable.get.id

    bindings.get(declarationId) should equal(Declaration)
    bindings.get(referenceId) should equal(Reference(declarationId))
  }

  test("MERGE should not accept an already bound variable") {


    intercept[VariableAlreadyDeclaredInScopeException](
      parseAndAnalyse("MATCH (a) MERGE (a)")
    )
  }

  test("ORDER BY can see variables in both scopes") {
    val (bindings, statement) =  parseAndAnalyse("MATCH (a) RETURN 123 AS x ORDER BY a.bar, x")


    val matchDeclaration = statement.findByClass[NodePattern].variable.get.id
    val returnDeclaration = statement.findByClass[AliasedReturnItem].variable.id
    val propRefId = statement.findByClass[Property].map.id
    val orderByVarReference = statement.findByClass[OrderBy].sortItems.last.expression.id

    bindings.get(matchDeclaration) should equal(Declaration)
    bindings.get(returnDeclaration) should equal(Declaration)
    bindings.get(propRefId) should equal(Reference(matchDeclaration))
    bindings.get(orderByVarReference) should equal(Reference(returnDeclaration))
  }

  private def testa[A: ClassTag](q: String, fa: A => LogicalVariable): Unit = test(q) {
    val (bindings, statement) =  parseAndAnalyse(q)

    val clauseId = fa(statement.findByClass[A]).id
    val returnVariableId = statement.findByClass[UnaliasedReturnItem].expression.id

    bindings.get(clauseId) should equal(Declaration)
    bindings.get(returnVariableId) should equal(Reference(clauseId))

  }


}

