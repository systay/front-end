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
class Scoping extends TopDownVisitor[Scope] {

  val readScope = new Scopes
  val writeScope = new Scopes

  private def setScopeTo(s: ASTNode, scope: Scope): Unit = {
    readScope.set(s.id, scope)
    writeScope.set(s.id, scope)
  }

  def gogogo(ast: ASTNode) = {
    TreeVisitor.visit(ast, new NormalScope(), this, BottomUpVisitor.empty)
  }

  override def visit(x: Any, scope: Scope): Scope = x match {
    // If this node is already scoped, we can just return that
    case x: ASTNode if readScope.contains(x.id) =>
      readScope.get(x.id)

    // WITH is a horizon between two scopes.
    // It reads from one and writes to the other
    case w: ProjectionClause =>
      readScope.set(w.id, scope)
      val newScope = new NormalScope()
      writeScope.set(w.id, newScope)
      w.orderBy.foreach { orderBy =>
        // ORDER BY needs special handling. It can read from the scope before the WITH,
        // but also from the scope which WITH writes. That is why we have
        // this special scope construct for WITH
        val weird = new BiScope(newScope, scope)
        readScope.set(orderBy.id, weird)
      }
      newScope

    case a: Foreach =>
      readScope.set(a.id, scope)
      readScope.set(a.expression.id, scope)
      val newScope = scope.createInnerScope()
      writeScope.set(a.id, newScope)
      newScope

    // This objects contains all the scoped parts of ALL/ANY/NONE/SINGLE
    case a: ScopeExpression =>
      val newScope = scope.createInnerScope()
      writeScope.set(a.id, newScope)
      newScope

    case u: Union =>
      val scope1 = scope.createInnerScope()
      val scope2 = scope.createInnerScope()
      setScopeTo(u.part, scope1)
      setScopeTo(u.query, scope2)
      scope1 // not really important, since we have already scoped all children

    case a: Clause =>
      readScope.set(a.id, scope)
      writeScope.set(a.id, scope)
      scope

    case _: IterablePredicateExpression => ???

    case e: Expression =>
      readScope.set(e.id, scope)
      scope

    case _ => scope
  }

}

object Scope {
  val idGen = new SequentialIdGen()
}

trait Scope {

  val id = Scope.idGen.id()
  def getVariable(name: String): Option[Variable]
  def createInnerScope(): Scope
  def addVariable(v: Variable): Unit
}

/**
  * This special scope is used for handling ORDER BY, which can read variables in two separate scopes
  */
class BiScope(a: Scope, b: Scope) extends Scope {
  override def getVariable(name: String): Option[Variable] = {
    val result = a.getVariable(name)
    if (result.nonEmpty)
      result
    else
      b.getVariable(name)
  }

  override def createInnerScope(): Scope = new NormalScope(Some(this))

  override def addVariable(v: Variable): Unit = throw new InternalException("can't add variables to a BiScope")

  override def toString = s"BiScope$id($a, $b)"
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

  override def toString: String = s"Scope($id)"
}

class Scopes extends Attribute[Scope]
