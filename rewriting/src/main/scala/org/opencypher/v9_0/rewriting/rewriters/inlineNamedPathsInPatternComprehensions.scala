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
package org.opencypher.v9_0.rewriting.rewriters

import org.opencypher.v9_0.expressions.{PathExpression, PatternElement, _}
import org.opencypher.v9_0.util.attribution.{Attributes, SameId}
import org.opencypher.v9_0.util.{Rewriter, bottomUp}

case class inlineNamedPathsInPatternComprehensions(attributes: Attributes) extends Rewriter {

  private val instance = bottomUp(Rewriter.lift {
    case expr @ PatternComprehension(Some(path), pattern, predicate, projection, _) =>
      val patternElement = pattern.element
      expr.copy(
        namedPath = None,
        predicate = predicate.map(inline(_, path, patternElement)),
        projection = inline(projection, path, patternElement)
      )(expr.position)(SameId(expr.id))
  })

  private def inline(expr: Expression, path: LogicalVariable, patternElement: PatternElement): Expression =
    expr.copyAndReplace(path) by {
      PathExpression(projectNamedPaths(attributes).patternPartPathExpression(patternElement))(expr.position)(attributes.copy(expr.id))
    }

  override def apply(v: AnyRef): AnyRef = instance(v)
}
