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

import org.opencypher.v9_0.util.test_helpers.CypherFunSuite

class TreeVisitorTest extends CypherFunSuite {

  trait Tree {
    val s: String
  }

  case class Leaf(s: String) extends Tree
  case class Branch(s: String, next: Tree) extends Tree
  case class Fork(s: String, lhs: Tree, rhs: Tree) extends Tree

  test("the test") {

    var result = ""

    object Down extends TopDownVisitor[String] {
      override def visit(x: Any, acc: String): String = {
        x match {
          case (tree: Tree) => result += s"Down ${tree.s}"
          case _ =>
        }
        acc
      }
    }

    object Up extends BottomUpVisitor {
      override def visit(x: Any): Unit =
        x match {
          case (tree: Tree) => result += s"Up   ${tree.s}"
          case _ =>
        }
    }

    val tree =
      Fork("A",
        Fork("B",
          Leaf("C"),
          Leaf("D")
        ),
        Branch("E", Leaf("F")))

    /*
        A
     B    E
    C D    F
     */

    TreeVisitor.visit(tree, "", Down, Up)
    result should equal(
        "Down A" +
        "Down B" +
        "Down C" +
        "Up   C" +
        "Down D" +
        "Up   D" +
        "Up   B" +
        "Down E" +
        "Down F" +
        "Up   F" +
        "Up   E" +
        "Up   A")
  }
}
