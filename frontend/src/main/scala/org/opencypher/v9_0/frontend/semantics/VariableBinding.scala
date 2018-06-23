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
import org.opencypher.v9_0.expressions.{LogicalVariable, NodePattern, RelationshipChain, Variable}
import org.opencypher.v9_0.util.attribution.{Attribute, Id}
import org.opencypher.v9_0.util.{ASTNode, InternalException}

/*
This class taking notes of variable declarations and references.
 */
class VariableBinder(variableBindings: VariableBindings) extends VariableBinding {
  override def bind(obj: ASTNode, scope: Scope, bindingMode: BindingMode): BindingMode = {
    def declareVar(v: LogicalVariable): Unit = {
      if(scope.getVariable(v.name).nonEmpty){
        throw new VariableAlreadyDeclaredInScopeException(v)
      }
      scope.addVariable(v)
      variableBindings.set(v.id, Declaration)
    }

    def declareIfNewVariable(v: LogicalVariable): Unit = {
      scope.getVariable(v.name) match {
        case Some(ref) =>
          variableBindings.set(v.id, Reference(ref.id))
        case None =>
          declareVar(v)
      }
    }

    (obj, bindingMode) match {
      case (_: Match, _) =>
        BindingAllowed

      case (ast: LogicalVariable, _) if variableBindings.contains(ast.id) =>
        bindingMode

      case (ast: LogicalVariable, BindingAllowed) =>
        declareIfNewVariable(ast)
        bindingMode

      case (ast: LogicalVariable, ReferenceOnly) =>
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
        relationshipChain.rightNode.variable.foreach(declareIfNewVariable)
        relationshipChain.element match {
          case leftNode: NodePattern =>
            leftNode.variable.foreach(declareIfNewVariable)
        }
        bindingMode

      case (NodePattern(Some(variable), _, _, _), RelationshipBindingOnly) if !variableBindings.contains(variable.id) =>
        declareVar(variable)
        bindingMode

      case (as : AliasedReturnItem, _) =>
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

object Declaration extends VariableUse

class VariableBindings extends Attribute[VariableUse]

// This is the Attribute[VariableUse] in a form that is easy to consume by the type algorithm
class Bindings(rootNode: ASTNode, val variableBindings: VariableBindings) {
  def declarationOf(v: LogicalVariable): Option[Variable] = {
    val id =
      variableBindings.get(v.id) match {
        case Declaration => throw new InternalException("this is a declaration")
        case Reference(x) => x
      }

    rootNode.treeFind[Variable] {
      case x: LogicalVariable => x.id == id
    }
  }
}

