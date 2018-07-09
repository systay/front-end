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
import org.opencypher.v9_0.expressions.{Expression, FunctionInvocation, FunctionName}
import org.opencypher.v9_0.frontend.semantics.SemanticTestHelper.{mockBinding, mockScoping, mockTypeExpectations}
import org.opencypher.v9_0.frontend.semantics.Types.{ListT, StringT}
import org.opencypher.v9_0.util.test_helpers.CypherFunSuite

class FunctionInvocationTypeJudgementTest extends CypherFunSuite with AstConstructionTestSupport {
  test("LABELS") {
    val types = new TypeJudgements
    val aVariable = varFor("a")
    val ast: Expression = FunctionInvocation(FunctionName("labels")(pos), aVariable)(pos)
    val bindings = new VariableBindings
    val expectations = new TypeExpectations
    val bindingsLookup = new BindingsLookup(ast, bindings)
    val typer = new TypeJudgementGenerator(types, bindingsLookup, expectations)

    bindings.set(aVariable.id, Declaration)
    expectations.set(aVariable.id, TypeInfo(nullable = true))
    new TreeWalker(mockScoping, mockBinding, mockTypeExpectations, typer).visit(ast)
    types.get(ast.id) should equal(TypeInfo(true, ListT(StringT)))
  }

}
