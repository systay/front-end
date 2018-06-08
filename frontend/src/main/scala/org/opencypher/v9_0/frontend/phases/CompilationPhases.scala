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
import org.opencypher.v9_0.ast.semantics.SemanticState
import org.opencypher.v9_0.rewriting.RewriterStepSequencer
import org.opencypher.v9_0.rewriting.rewriters.{IfNoParameter, LiteralExtraction}
import org.opencypher.v9_0.util.attribution.Attributes

object CompilationPhases {

  def parsing(sequencer: String => RewriterStepSequencer,
              attributes: Attributes,
              literalExtraction: LiteralExtraction = IfNoParameter): Transformer[BaseContext, BaseState, BaseState] =
    Parsing(attributes).adds(BaseContains[Statement]) andThen
      SyntaxDeprecationWarnings andThen
      PreparatoryRewriting(attributes) andThen
      SemanticAnalysis(warn = true).adds(BaseContains[SemanticState]) andThen
      AstRewriting(sequencer, literalExtraction, attributes = attributes)

  def lateAstRewriting(attributes: Attributes): Transformer[BaseContext, BaseState, BaseState] =
    SemanticAnalysis(warn = false) andThen
      Namespacer(attributes) andThen
      transitiveClosure(attributes) andThen
      rewriteEqualityToInPredicate(attributes) andThen
      CNFNormalizer(attributes) andThen
      LateAstRewriting(attributes)
}
