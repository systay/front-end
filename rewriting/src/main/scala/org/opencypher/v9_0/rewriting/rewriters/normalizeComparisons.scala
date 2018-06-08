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

import org.opencypher.v9_0.expressions._
import org.opencypher.v9_0.util.attribution.{Attributes, SameId}
import org.opencypher.v9_0.util.{Rewriter, topDown}

case class normalizeComparisons(attr: Attributes) extends Rewriter {

  private val inner: Rewriter = copyVariables(attr)

  private val instance: Rewriter = topDown(Rewriter.lift {
    case c: NotEquals =>
      NotEquals(c.lhs.endoRewrite(inner), c.rhs.endoRewrite(inner))(c.position)(SameId(c.id))
    case c: Equals =>
      Equals(c.lhs.endoRewrite(inner), c.rhs.endoRewrite(inner))(c.position)(SameId(c.id))
    case c: LessThan =>
      LessThan(c.lhs.endoRewrite(inner), c.rhs.endoRewrite(inner))(c.position)(SameId(c.id))
    case c: LessThanOrEqual =>
      LessThanOrEqual(c.lhs.endoRewrite(inner), c.rhs.endoRewrite(inner))(c.position)(SameId(c.id))
    case c: GreaterThan =>
      GreaterThan(c.lhs.endoRewrite(inner), c.rhs.endoRewrite(inner))(c.position)(SameId(c.id))
    case c: GreaterThanOrEqual =>
      GreaterThanOrEqual(c.lhs.endoRewrite(inner), c.rhs.endoRewrite(inner))(c.position)(SameId(c.id))
    case c: InvalidNotEquals =>
      InvalidNotEquals(c.lhs.endoRewrite(inner), c.rhs.endoRewrite(inner))(c.position)(SameId(c.id))
    case c: HasLabels if c.labels.size > 1 =>
      val hasLabels = c.labels.map(l => HasLabels(c.expression.endoRewrite(inner), Seq(l))(c.position)(attr.copy(c.id)))
      Ands(hasLabels.toSet)(c.position)(attr.copy(c.id))
  })

  override def apply(that: AnyRef): AnyRef = instance(that)
}
