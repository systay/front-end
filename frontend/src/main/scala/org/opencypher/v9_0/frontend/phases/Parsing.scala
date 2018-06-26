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
package org.opencypher.v9_0.frontend.phases

import org.opencypher.v9_0.ast.Statement
import org.opencypher.v9_0.frontend.phases.CompilationPhaseTracer.CompilationPhase.PARSING
import org.opencypher.v9_0.parser.CypherParser

case object Parsing extends Phase[BaseContext, BaseState, BaseState] {

  override def process(in: BaseState, ctx: BaseContext): BaseState = {
    val parseResult = CypherParser.parse(in.queryText, ctx.astIdGen, in.startPosition)
    in.
      withStatement(parseResult.statement).
      withPositions(parseResult.positions)
  }

  override val phase = PARSING

  override val description = "parse text into an AST object"

  override def postConditions = Set(BaseContains[Statement])
}
