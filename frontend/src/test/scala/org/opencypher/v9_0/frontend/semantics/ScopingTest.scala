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

import org.mockito.ArgumentMatchers._
import org.mockito.Mockito._
import org.opencypher.v9_0.ast._
import org.opencypher.v9_0.expressions._
import org.opencypher.v9_0.parser.CypherParser
import org.opencypher.v9_0.util.attribution.{Attributes, SequentialIdGen}
import org.opencypher.v9_0.util.test_helpers.CypherFunSuite

class ScopingTest extends CypherFunSuite {

  val binder = mock[VariableBinder]
  when(binder.bind(any(), any(), any())).thenReturn(BindingAllowed)
  val typeExpecting = mock[TypeExpecting]

  private def parseAndAnalyse(q: String) = {
    val scopes = new Scopes
    val parser = new CypherParser(Attributes(new SequentialIdGen()))
    val x = parser.parse(q)


    new TreeWalker(new Scoper(scopes), binder, typeExpecting).visit(x.statement)
    (scopes, x.statement)
  }

  test("WITH creates sibling scopes") {
    val (scopes, statement) = parseAndAnalyse("MATCH (a) WITH a.prop as x RETURN *")
    val matchNode = statement.findByClass[Match]
    val returnNode = statement.findByClass[Return]

    val matchScope = scopes.get(matchNode.id)
    val returnScope = scopes.get(returnNode.id)

    matchScope should not equal returnScope
    matchScope.popScope() should equal(returnScope.popScope())
  }

  test("ORDER BY lives in a special scope") {
    val (scopes, statement) = parseAndAnalyse("MATCH (a) WITH a ORDER BY a.prop RETURN *")


    val orderBy = statement.findByClass[OrderBy]
    val orderByScope = scopes.get(orderBy.id)

    orderByScope shouldBe a[BiScope]

    val matchNode = statement.findByClass[Match]
    val returnNode = statement.findByClass[Return]

    val matchScope = scopes.get(matchNode.id)
    val returnScope = scopes.get(returnNode.id)

    val biScope = orderByScope.asInstanceOf[BiScope]
    biScope.firstScope should equal(returnScope)
    biScope.secondScope should equal(matchScope)
  }

  test("FOREACH creates inner scopes for the updates") {
    val (scopes, statement) = parseAndAnalyse("CREATE (a) FOREACH(x in [1,2,3] | SET a.prop = x) REMOVE a.prop")


    val create = statement.findByClass[Create]
    val set = statement.findByClass[SetClause]
    val literalList = statement.findByClass[ListLiteral]
    val foreach = statement.findByClass[Foreach]
    val delete = statement.findByClass[Remove]

    val createScope = scopes.get(create.id)
    val setScope = scopes.get(set.id)
    val listScope = scopes.get(literalList.id)
    val foreachScope = scopes.get(foreach.id)
    val deleteScope = scopes.get(delete.id)

    createScope should equal(foreachScope)
    createScope should equal(deleteScope)
    setScope.popScope() should equal(createScope)
    listScope should equal(createScope)
  }

  test("ListComprehension") {
    val (scopes, statement) = parseAndAnalyse("RETURN [ x in $param | x + 12 ]")

    val returnClass = statement.findByClass[Return]
    val listComp = statement.findByClass[ListComprehension]
    val add = statement.findByClass[Add]

    val returnClassScope = scopes.get(returnClass.id)
    val listCompScope = scopes.get(listComp.id)
    val addScope = scopes.get(add.id)

    returnClassScope should equal(listCompScope)
    addScope.popScope() should equal(listCompScope)
  }

  test("PatternComprehension") {
    val (scopes, statement) = parseAndAnalyse("MATCH (a: Person) RETURN [ (a)-[:FRIEND]->(b) | b.name ]")

    val returnClass = statement.findByClass[Return]
    val pattComp = statement.findByClass[PatternComprehension]
    val relPattern = statement.findByClass[RelationshipsPattern]
    val projection = statement.findByClass[Property]

    val returnClassScope = scopes.get(returnClass.id)
    val pattCompScope = scopes.get(pattComp.id)
    val projectionScope = scopes.get(projection.id)
    val relPatternScope = scopes.get(relPattern.id)

    projectionScope should equal(relPatternScope)
    relPatternScope.popScope() should equal(pattCompScope)
  }

  test("UNION") {
    val (scopes, statement) = parseAndAnalyse("RETURN 1 as X UNION RETURN '1' AS X")

    val integer = statement.findByClass[SignedIntegerLiteral]
    val string = statement.findByClass[StringLiteral]

    val intScope = scopes.get(integer.id)
    val strScope = scopes.get(string.id)

    intScope should not equal(strScope)
    intScope.popScope() should equal(strScope.popScope())
  }

}
