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
import org.opencypher.v9_0.util.attribution.{Attribute, SequentialIdGen}
import org.opencypher.v9_0.util.{ASTNode, InternalException}

/**
  * Scopes in Cypher are a little different from scopes in a language such as Java or Scala.
  *
  * In Java, you have scopes that can contain other scopes inside of them. These scopes exist in Cypher
  * as well, but then you also have weird behaviours around WITH. These are modeled by WITH reading from one scope
  * and writing to another.
  *
  * Clauses can read from one and write to another scope, but expressions only read from scopes
  *
  * Objects of this class should not be re-used.
  */

class Scoper(scopes: Scopes) extends Scoping {

  override def scope(ast: ASTNode, currentScope: Scope): ScopingResult =
    ast match {
        // if we are dealing with ORDER BY and the bi-scope has already been defined.
      case ast: ASTNode if scopes.contains(ast.id) =>
        changeScopeForChildren(scopes.get(ast.id), currentScope)

        // WITH or RETURN
      case ast: ProjectionClause =>
        val newScope = currentScope.popScope().createInnerScope()
        ast.orderBy.foreach { orderBy =>
          val newBiScope = new BiScope(newScope, currentScope)
          scopes.set(orderBy.id, newBiScope)
        }
        scopes.set(ast.id, currentScope)
        changeScopeForSiblings(newScope)

      case ast: Foreach =>
        createNewSubScope(ast, currentScope, scopes, ast.expression)

      case ast: ScopeExpression =>
        createNewSubScope(ast, currentScope, scopes)

      case ast: Union =>
        scopes.set(ast.query.id, currentScope.createInnerScope())
        scopes.set(ast.part.id, currentScope.createInnerScope())
        changeScopeForSiblings(currentScope)

      case _ =>
        scopes.set(ast.id, currentScope)
        ScopingResult(None, None)
    }

  private def createNewSubScope(t: ASTNode, currentScope: Scope, scopes: Scopes, children: ASTNode*) = {
    val childScope = currentScope.createInnerScope()
    children.foreach(child => scopes.set(child.id, currentScope))
    scopes.set(t.id, currentScope)
    changeScopeForChildren(childScope, currentScope)
  }

  private def changeScopeForChildren(children: Scope, pop: Scope) = ScopingResult(Some(children), Some(pop))
  private def changeScopeForSiblings(scope: Scope) = ScopingResult(None, Some(scope))

}

object Scope {
  val idGen = new SequentialIdGen
}

trait Scope {
  val id = Scope.idGen.id()
  def getVariable(name: String): Option[Variable]
  def createInnerScope(): Scope
  def addVariable(v: Variable): Unit
  def popScope(): Scope
}

/**
  * This special scope is used for handling ORDER BY, which can read variables in two separate scopes
  */
class BiScope(val firstScope: Scope, val secondScope: Scope) extends Scope {
  override def getVariable(name: String): Option[Variable] = {
    val result = firstScope.getVariable(name)
    if (result.nonEmpty)
      result
    else
      secondScope.getVariable(name)
  }

  override def createInnerScope(): Scope = new NormalScope(Some(this))

  override def toString = s"BiScope${id.x}($firstScope, $secondScope)"

  override def addVariable(v: Variable): Unit = throw new InternalException("can't add variables to a BiScope")

  override def popScope(): Scope = throw new InternalException("can't pop scope on a BiScope")
}

class NormalScope(parent: Option[Scope] = None, var locals: Set[Variable] = Set.empty) extends Scope {

  override def createInnerScope(): Scope = {
    new NormalScope(Some(this))
  }

  override def getVariable(name: String): Option[Variable] = {
    val local: Option[Variable] = locals.collectFirst {
      case v if v.name == name => v
    }

    (local, parent) match {
      case (x: Some[_], _) => x
      case (None, None) => None
      case (None, Some(p)) => p.getVariable(name)
    }
  }

  override def addVariable(v: Variable): Unit = locals += v

  override def toString: String = {
    val parentS = parent.map(_.toString).getOrElse("")
    s" Scope${id.x}($parentS)"
  }

  override def popScope(): Scope = parent.getOrElse(throw new InternalException("have no scope to pop at this location"))
}

class Scopes extends Attribute[Scope]
