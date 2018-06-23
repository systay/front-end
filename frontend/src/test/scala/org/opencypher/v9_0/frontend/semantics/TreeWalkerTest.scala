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
import org.opencypher.v9_0.util.ASTNode
import org.opencypher.v9_0.util.attribution.{Attributes, SequentialIdGen}
import org.opencypher.v9_0.util.test_helpers.CypherFunSuite

class TreeWalkerTest extends CypherFunSuite {
  val binder = mock[VariableBinder]

  test("should visit things in the correct order") {
    var seenNodes = Seq[Class[_]]()
    val scoping = new Scoping {
      override def scope(ast: ASTNode, incoming: Scope): ScopingResult = {
        seenNodes = seenNodes :+ ast.getClass
        ScopingResult(None, None)
      }
    }

    val parser = new CypherParser(Attributes(new SequentialIdGen()))
    val x = parser.parse("MATCH (a) WITH a.prop as x ORDER BY a.foo, x RETURN *")
    new TreeWalker(scoping, binder, mock[TypeExpecting]).visit(x.statement)


    // These are the nodes in the order they are seen going down
    seenNodes should equal(Seq(
      classOf[Query],
      classOf[SingleQuery],
      classOf[Match],
      classOf[Pattern],
      classOf[EveryPath],
      classOf[NodePattern],
      classOf[Variable],
      classOf[With],
      classOf[ReturnItems],
      classOf[AliasedReturnItem],
      classOf[Property],
      classOf[Variable],
      classOf[PropertyKeyName],
      classOf[Variable],
      classOf[OrderBy],
      classOf[AscSortItem],
      classOf[Property],
      classOf[Variable],
      classOf[PropertyKeyName],
      classOf[AscSortItem],
      classOf[Variable],
      classOf[Return],
      classOf[ReturnItems])
    )
  }
}
