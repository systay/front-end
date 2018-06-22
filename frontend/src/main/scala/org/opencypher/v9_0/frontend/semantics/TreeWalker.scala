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

class TreeWalker(scoping: Scoping, variableBinding: VariableBinding) {
  def visit(astRoot: ASTNode): Unit = {
    val todo = new mutable.ArrayStack[Move]()
    todo.push(Down(astRoot))
    val scopeForEntireQuery = new NormalScope()

    var currentScope = scopeForEntireQuery.createInnerScope()
    var currentBindingMode: BindingMode = ReferenceOnly

    while (todo.nonEmpty) {
      val currentNode = todo.pop()

      currentNode match {
        case Down(ast: ASTNode) =>
          val scopingResult = scoping.scope(ast, currentScope)
          todo.push(Up(ast, scopingResult.comingUpScope, Some(currentBindingMode)))
          currentBindingMode = variableBinding.bind(ast, currentScope, currentBindingMode)

          scopingResult.changeCurrentScopeTo.foreach(s => currentScope = s)
          ast.reverseChildren.foreach(child =>
            todo.push(Down(child))
          )

        case Down(ast) =>
          ast.reverseChildren.foreach(child =>
            todo.push(Down(child))
          )

        case Up(ast: ASTNode, maybeScope, maybeBindingMode) =>
          maybeScope.foreach(s => currentScope = s)
          maybeBindingMode.foreach(s => currentBindingMode = s)
      }
    }
  }
}

trait Move

case class Down(data: AnyRef) extends Move

case class Up(data: AnyRef, s: Option[Scope], v: Option[BindingMode]) extends Move

case class ScopingResult(changeCurrentScopeTo: Option[Scope], comingUpScope: Option[Scope])

trait Scoping {
  def scope(ast: ASTNode, incoming: Scope): ScopingResult
}

trait VariableBinding {
  def bind(ast: ASTNode, scope: Scope, bindingMode: BindingMode): BindingMode
}

// When visiting the tree, we need to sometimes remember if
trait BindingMode
case object BindingAllowed extends BindingMode
case object ReferenceOnly extends BindingMode
