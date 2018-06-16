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

import org.opencypher.v9_0.util.ASTNode
import org.opencypher.v9_0.util.Foldable._

import scala.collection.mutable

object TreeVisitor {
  def visit[A](treeRoot: Any,
               initialValue: A,
               topDown: TopDownVisitor[A],
               bottomUp: BottomUpVisitor): Unit = {
    val todo = mutable.ArrayStack[(Any, Direction[A])]((treeRoot, Down(initialValue)))

    while (todo.nonEmpty) {
      val current = todo.pop()
      current match {
        case (curr, d@Down(state)) =>
          val nextState = if(curr.isInstanceOf[ASTNode]) {
            val newState = topDown.visit(curr, state)
            todo.push((curr, Up()))
            Down(newState)
          } else d

          // If we want to change the tree rewriting framework - change this
          val children: Iterator[AnyRef] = curr.reverseChildren
          children.foreach { child =>
            todo.push(child, nextState)
          }

        case (curr, Up()) =>
          bottomUp.visit(curr)
      }
    }
  }
}

trait Direction[T]

case class Up[T]() extends Direction[T] // Hack because I don't understand Scala types.
case class Down[T](a: T) extends Direction[T]

trait TopDownVisitor[T] {
  def visit(x: Any, acc: T): T
}

object TopDownVisitor {
  def compose[A, B](a: TopDownVisitor[A],
                    b: TopDownVisitor[B],
                    initA: A,
                    initB: B): TopDownVisitor[(A,B)] = ???

  def empty = new TopDownVisitor[Unit] {
    override def visit(x: Any, acc: Unit): Unit = {}
  }
}

object BottomUpVisitor {
  def empty = new BottomUpVisitor {
    override def visit(x: Any): Unit = {}
  }
}
trait BottomUpVisitor {
  def visit(x: Any): Unit
}

class ComposedTopDown[A, B](a: TopDownVisitor[A], b: TopDownVisitor[B]) extends TopDownVisitor[(A, B)] {
  override def visit(x: Any, acc: (A, B)): (A, B) = {
    val a2 = a.visit(x, acc._1)
    val b2 = b.visit(x, acc._2)
    (a2, b2)
  }
}

