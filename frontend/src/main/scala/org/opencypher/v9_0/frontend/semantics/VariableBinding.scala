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
import org.opencypher.v9_0.expressions.{LogicalVariable, NodePattern, RelationshipChain}
import org.opencypher.v9_0.util.attribution.{Attribute, Id}
import org.opencypher.v9_0.util.{ASTNode, InternalException}

import scala.annotation.tailrec
import scala.collection.mutable

/*
This class taking notes of variable declarations and references.
 */
class VariableBinder(variableBindings: VariableBindings, scopes: Scopes) extends VariableBinding {


  // When, during variable binding we are trying to declare one of our children,
  // it might not yet have been scoped. This marker reminds us to declare it in
  // the scope, once the object has been scoped correctly.
  private val rememberToDeclare = new mutable.HashSet[Id]()
  private val rememberToReferenceOrDeclare = new mutable.HashSet[Id]()


  override def bind(obj: ASTNode, bindingMode: BindingMode): BindingMode = {
    def declareVar(v: LogicalVariable): Unit =
      scopes.optionalGet(v.id) match {
        case None =>
          // We can't declare it until we have a scope
          rememberToDeclare.add(v.id)

        case Some(scope) =>
          scope.addVariable(v)
          variableBindings.set(v.id, Declaration)
      }

    def referenceOrDeclare(v: LogicalVariable): Unit =
      scopes.optionalGet(v.id) match {
        case None =>
          // We have to wait with actually doing this until we have a scope
          rememberToReferenceOrDeclare.add(v.id)

        case Some(scope) =>
          scope.getVariable(v.name) match {
            case Some(ref) =>
              variableBindings.set(v.id, Reference(ref.id))
            case None =>
              declareVar(v)
          }
      }

    (obj, bindingMode) match {
      case (m: Match, _) =>
        BindingAllowed(m.optional)

      case (_: Create|_:Merge, _) =>
        RelationshipBindingOnly

      case (ast: LogicalVariable, _) if rememberToDeclare(ast.id) =>
        declareVar(ast)
        bindingMode

      case (ast: LogicalVariable, _) if rememberToReferenceOrDeclare(ast.id) =>
        referenceOrDeclare(ast)
        bindingMode

      case (ast: LogicalVariable, _) if variableBindings.contains(ast.id) =>
        bindingMode

      case (ast: LogicalVariable, _: BindingAllowed) =>
        referenceOrDeclare(ast)
        bindingMode

      case (ast: LogicalVariable, ReferenceOnly) =>
        val scope = scopes.get(ast.id)
        scope.getVariable(ast.name) match {
          case Some(ref) =>
            variableBindings.set(ast.id, Reference(ref.id))
          case None =>
            throw new VariableNotDeclaredError(ast)
        }
        bindingMode

      case (unwind: Unwind, _) =>
        declareVar(unwind.variable)
        bindingMode

      case (load: LoadCSV, _) =>
        declareVar(load.variable)
        bindingMode

      case (foreach: Foreach, _) =>
        declareVar(foreach.variable)
        bindingMode

      case (relationshipChain: RelationshipChain, RelationshipBindingOnly) =>
        relationshipChain.relationship.variable.foreach(declareVar)
        relationshipChain.rightNode.variable.foreach(referenceOrDeclare)
        relationshipChain.element match {
          case leftNode: NodePattern =>
            leftNode.variable.foreach(referenceOrDeclare)
        }
        bindingMode

      case (NodePattern(Some(variable), _, _, _), RelationshipBindingOnly)
        if !variableBindings.contains(variable.id) && !rememberToReferenceOrDeclare(variable.id) =>
        declareVar(variable)
        bindingMode

      case (as: AliasedReturnItem, _) =>
        declareVar(as.variable)
        bindingMode

      case (_: Create | _: Merge, _) =>
        RelationshipBindingOnly

      case _ =>
        bindingMode
    }
  }
}

sealed trait VariableUse

case class Reference(id: Id) extends VariableUse
case object Declaration extends VariableUse

class VariableBindings extends Attribute[VariableUse]

// This is the Attribute[VariableUse] in a form that is easy to consume by the type algorithm
class BindingsLookup(rootNode: ASTNode, variableBindings: VariableBindings) {
  def declarationOf(v: LogicalVariable): LogicalVariable = {
    val id = getDeclaration(v.id)

    rootNode.treeFind[LogicalVariable] {
      case x: LogicalVariable => x.id == id
    }.getOrElse(throw new InternalException("Was not able to find the declaration of this variable"))
  }

  def isDeclaration(v: LogicalVariable): Boolean = variableBindings.get(v.id) == Declaration

  @tailrec
  private def getDeclaration(id: Id): Id = {
    variableBindings.get(id) match {
      case Declaration => id
      case Reference(x) => getDeclaration(x)
    }
  }
}

