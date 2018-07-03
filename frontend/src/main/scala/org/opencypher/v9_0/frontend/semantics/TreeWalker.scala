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

import org.opencypher.v9_0.expressions.Expression
import org.opencypher.v9_0.util.ASTNode
import org.opencypher.v9_0.util.Foldable._
import org.opencypher.v9_0.util.attribution.Attribute.DEBUG

import scala.collection.mutable

/*
This class is responsible for walking the AST tree and knowing how to stitch together the different
parts of semantic analysis together.

Traversal of the tree is not done using recursion, because we encounter trees large enough to cause a problem
with the JVM-stack. Instead, a manual stack and a good old while loop is used.

The stages (scoping, binding, etc) get to visit nodes at different phases of the tree walking.
 */
class TreeWalker(scoping: Scoping,
                 variableBinding: VariableBinding,
                 typeExpecting: TypeExpecting,
                 bottomUpVisitor: BottomUpVisitor) {
  def visit(astRoot: ASTNode): Unit = {
    if(DEBUG) {
      println(TreeWithId2String.toString(astRoot))
    }
    val todo = new mutable.ArrayStack[Move]()
    todo.push(Down(astRoot))
    val scopeForEntireQuery = new NormalScope()

    var currentScope = scopeForEntireQuery.createInnerScope()
    var currentBindingMode: BindingMode = ReferenceOnly

    def analysisGoingDown(ast: ASTNode): Unit = {
      debugPrint("DOWN", ast)
      val bindingMode = currentBindingMode

      val scopingResult = scoping.scope(ast, currentScope)
      todo.push(Up(ast, scopingResult.comingUpScope, bindingMode))
      currentBindingMode = variableBinding.bind(ast, bindingMode)
      typeExpecting.visit(ast, bindingMode)

      scopingResult.changeCurrentScopeTo.foreach { s =>
        currentScope = s
      }
    }


    while (todo.nonEmpty) {
      val currentNode = todo.pop()

      currentNode match {
        case Down(obj) =>
          obj match {
            case node: ASTNode => analysisGoingDown(node)
            case _ =>
          }

          obj.reverseChildren.foreach { child =>
            todo.push(Down(child))
          }


        case Up(node, maybeScope, bindingMode) =>
          debugPrint("UP", node)
          maybeScope.foreach { s =>
            currentScope = s
          }

          node match {
            case e: Expression =>
                bottomUpVisitor.visit(e)
            case _ =>
          }

          currentBindingMode = bindingMode
      }
    }
  }

  private def debugPrint(verb: String, value: ASTNode): Unit = if(DEBUG) {
    val c = this.getClass.getSimpleName.padTo(20, " ").mkString
    val v = verb.padTo(14, " ").mkString
    val i = value.id.x.toString.padTo(4, " ").mkString

    println(s"$c$v$i${value.getClass.getSimpleName}")
  }

}

trait Move
case class Down(data: AnyRef) extends Move
case class Up(data: ASTNode, s: Option[Scope], v: BindingMode) extends Move


case class ScopingResult(changeCurrentScopeTo: Option[Scope], comingUpScope: Option[Scope])

trait Scoping {
  def scope(ast: ASTNode, incoming: Scope): ScopingResult
}

trait VariableBinding {
  def bind(ast: ASTNode, bindingMode: BindingMode): BindingMode
}

trait TypeExpecting {
  def visit(ast: ASTNode, bindingMode: BindingMode): Unit
}

trait BottomUpVisitor {
  self =>

  def visit(e: ASTNode): Unit

  def andThen(other: BottomUpVisitor): BottomUpVisitor = new BottomUpVisitor {
    override def visit(e: ASTNode): Unit = {
      self.visit(e)
       other.visit(e)
    }
  }
}


// When visiting the tree, we need to sometimes remember if
trait BindingMode
case class BindingAllowed(nullable: Boolean) extends BindingMode
case object ReferenceOnly extends BindingMode
case object RelationshipBindingOnly extends BindingMode