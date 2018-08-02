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
    todo.push(Down(astRoot, ReferenceOnly))
    val scopeForEntireQuery = new NormalScope()

    var currentScope = scopeForEntireQuery.createInnerScope()

    def analysisGoingDown(ast: ASTNode, varContext: VariableContext): VariableContext = {
      debugPrint("DOWN", ast)

      val scopingResult = scoping.scope(ast, currentScope)
      todo.push(Up(ast, scopingResult.comingUpScope, varContext))
      val newVarContext = variableBinding.bind(ast, varContext)
      typeExpecting.visit(ast, varContext)

      scopingResult.changeCurrentScopeTo.foreach { s =>
        currentScope = s
      }
      newVarContext
    }


    while (todo.nonEmpty) {
      val currentNode = todo.pop()

      currentNode match {
        case Down(obj, varContext) =>
          val childContext =
            obj match {
              case node: ASTNode => analysisGoingDown(node, varContext)
              case _ => varContext
            }

          obj.reverseChildren.foreach { child =>
            todo.push(Down(child, childContext))
          }


        case Up(node, maybeScope, varContext) =>
          debugPrint("UP", node)
          maybeScope.foreach { s =>
            currentScope = s
          }

          node match {
            case e: ASTNode =>
                bottomUpVisitor.visit(e, varContext)
            case _ =>
          }
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
case class Down(data: AnyRef, v: VariableContext) extends Move
case class Up(data: ASTNode, s: Option[Scope], v: VariableContext) extends Move


case class ScopingResult(changeCurrentScopeTo: Option[Scope], comingUpScope: Option[Scope])

trait Scoping {
  def scope(ast: ASTNode, incoming: Scope): ScopingResult
}

trait VariableBinding {
  def bind(ast: ASTNode, variableContext: VariableContext): VariableContext
}

trait TypeExpecting {
  def visit(ast: ASTNode, variableContext: VariableContext): Unit
}

trait BottomUpVisitor {
  self =>

  def visit(e: ASTNode, variableContext: VariableContext): Unit

  def andThen(other: BottomUpVisitor): BottomUpVisitor = new BottomUpVisitor {
    override def visit(e: ASTNode, variableContext: VariableContext): Unit = {
      self.visit(e, variableContext)
      other.visit(e, variableContext)
    }
  }
}

// When visiting the tree, we need to remember the context of a variable to decide
// if e.g. it is be a declaration or a reference
trait VariableContext
case class InMatch(nullable: Boolean) extends VariableContext
case object ReferenceOnly extends VariableContext
case object InMergeOrCreate extends VariableContext