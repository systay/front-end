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

import org.opencypher.v9_0.ast.Statement
import org.opencypher.v9_0.expressions.Variable
import org.opencypher.v9_0.util.attribution.{Attribute, Id}
import org.opencypher.v9_0.util.{ASTNode, InternalException}


/**
  * After this phase, every Variable instance in the AST tree will be annotated with information about
  * whether a variable is declared or bound to something earlier
  */
object VariableBinding {

  class VariableBinder(readScope: Scopes, writeScope: Scopes) extends TopDownVisitor[Unit] {
    val bindings = new VariableBindings

    override def visit(x: Any, acc: Unit): Unit = x match {
      case v: Variable if bindings.contains(v.id) =>
        val scope = readScope.get(v.id)
        val variableDeclaration: Option[Variable] = scope.getVariable(v.name)
        variableDeclaration match {
          case None =>
            bindings.set(v.id, Declaration(v.id))
            scope.addVariable(v)
          case Some(other) =>
            bindings.set(v.id, Resolution(other.id))
        }

      case _ =>

//      case f: Foreach =>
//        ???
//        val scope = writeScope.get(f.id)
//        bindings.set(f.variable.id, Declaration(f.variable.id))
//        scope.locals += f.variable

    }
  }

  def doIt(statement: Statement, readScope: Scopes, writeScope: Scopes): VariableBindings = {
    val bindings = new VariableBindings
    val binder = new VariableBinder(readScope, writeScope)
    binder.visit(statement, {})
    bindings
  }
}

sealed trait VariableUse
case class Resolution(id: Id) extends VariableUse
case class Declaration(id: Id) extends VariableUse

class VariableBindings extends Attribute[VariableUse]

// This is the Attribute[VariableUse] in a form that is easy to consume by the type algorithm
class Bindings(ast: ASTNode, val variableBindings: VariableBindings) {
  def declarationOf(v: Variable): Option[Variable] = {
    val id =
      variableBindings.get(v.id) match {
        case _:Declaration => throw new InternalException("this is a declaration")
        case Resolution(x) => x
      }

    ast.treeFind[Variable] {
      case x: Variable => x.id == id
    }
  }
}