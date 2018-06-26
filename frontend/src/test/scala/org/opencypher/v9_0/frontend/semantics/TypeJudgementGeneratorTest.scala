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

import org.opencypher.v9_0.ast.UnaliasedReturnItem
import org.opencypher.v9_0.frontend.semantics.Types._
import org.opencypher.v9_0.parser.CypherParser
import org.opencypher.v9_0.util.attribution.SequentialIdGen
import org.opencypher.v9_0.util.test_helpers.CypherFunSuite

class TypeJudgementGeneratorTest extends CypherFunSuite {

  private def testa(q: String, expectedTypeInfo: TypeInfo): Unit = test(q) {
    val x = CypherParser.parse(q, new SequentialIdGen())
    val bindings = new VariableBindings
    val expectations = new TypeExpectations
    val types = new TypeJudgements

    val typer = new TypeJudgementGenerator(types, new BindingsLookup(x.statement, bindings), expectations)
    val scopes = new Scopes
    val scoper = new Scoper(scopes)
    val binder = new VariableBinder(bindings, scopes)
    val generator = new TypeExpectationsGenerator(expectations, types)

    new TreeWalker(scoper, binder, generator, typer).visit(x.statement)

    val expressionId = x.statement.findByClass[UnaliasedReturnItem].expression.id
    types.get(expressionId) should equal(expectedTypeInfo)
  }


//  testa("MATCH (a) RETURN (a)-->()", TypeInfo(Set(ListType(PathType)), nullable = true))
//  testa("MATCH (a) RETURN a", TypeInfo(Set(NodeType), nullable = false))
//  testa("RETURN [1,2,3]", TypeInfo(Set(ListType(IntegerType)), nullable = false))
  testa("MATCH (a) WITH a AS a RETURN a", NonNullableType(NodeType))
}
