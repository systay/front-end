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

/*
This class is responsible for walking the AST tree and knowing how to stitch together the different
parts of semantic analysis together.
 */
class TreeWalker(scoping: Scoping,
                 variableBinding: VariableBinding,
                 typeExpecting: TypeExpecting) {
  def visit(astRoot: ASTNode): Unit = {
    val todo = new mutable.ArrayStack[Move]()
    todo.push(Down(astRoot))
    val scopeForEntireQuery = new NormalScope()

    var currentScope = scopeForEntireQuery.createInnerScope()
    var currentBindingMode: BindingMode = ReferenceOnly

    def doSemanticAnalysis(ast: ASTNode): Unit = {
      val scopingResult = scoping.scope(ast, currentScope)
      todo.push(Up(ast, scopingResult.comingUpScope, currentBindingMode))
      currentBindingMode = variableBinding.bind(ast, currentScope, currentBindingMode)
      scopingResult.changeCurrentScopeTo.foreach { s =>
        currentScope = s
      }
      typeExpecting.visit(ast)
    }


    while (todo.nonEmpty) {
      val currentNode = todo.pop()

      currentNode match {
        case Down(obj) =>
          obj match {
            case node: ASTNode => doSemanticAnalysis(node)
            case _ =>
          }

          obj.reverseChildren.foreach { child =>
            todo.push(Down(child))
          }


        case Up(_, maybeScope, bindingMode) =>
          maybeScope.foreach { s =>
            currentScope = s
          }
          currentBindingMode = bindingMode
      }
    }
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
  def bind(ast: ASTNode, scope: Scope, bindingMode: BindingMode): BindingMode
}

trait TypeExpecting {
  def visit(ast: ASTNode): Unit
}



// When visiting the tree, we need to sometimes remember if
trait BindingMode
case object BindingAllowed extends BindingMode
case object ReferenceOnly extends BindingMode
case object RelationshipBindingOnly extends BindingMode