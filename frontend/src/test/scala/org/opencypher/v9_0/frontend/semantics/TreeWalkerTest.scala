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
import org.opencypher.v9_0.parser.CypherParser
import org.opencypher.v9_0.util.ASTNode
import org.opencypher.v9_0.util.attribution.SequentialIdGen
import org.opencypher.v9_0.util.test_helpers.CypherFunSuite

class TreeWalkerTest extends CypherFunSuite with AstConstructionTestSupport {
  val binder = mock[VariableBinder]

  test("should visit things in the correct order") {
    var seenNodes = Seq[Class[_]]()
    val scoping = new Scoping {
      override def scope(ast: ASTNode, incoming: Scope): ScopingResult = {
        seenNodes = seenNodes :+ ast.getClass
        ScopingResult(None, None)
      }
    }

    val x = CypherParser.parse("MATCH (a) WITH a.prop as x ORDER BY a.foo, x RETURN *", new SequentialIdGen())
    new TreeWalker(scoping, binder, mock[TypeExpecting], mock[BottomUpVisitor]).visit(x.statement)


    // These are the nodes in the order they are seen going down
    seenNodes should equal(Seq(
      classOf[Query],
      classOf[SingleQuery],
      classOf[Match],
      classOf[Pattern],
      classOf[EveryPath],
      classOf[NodePattern],
      classOf[Variable],
      classOf[With],
      classOf[ReturnItems],
      classOf[AliasedReturnItem],
      classOf[Property],
      classOf[Variable],
      classOf[PropertyKeyName],
      classOf[Variable],
      classOf[OrderBy],
      classOf[AscSortItem],
      classOf[Property],
      classOf[Variable],
      classOf[PropertyKeyName],
      classOf[AscSortItem],
      classOf[Variable],
      classOf[Return],
      classOf[ReturnItems])
    )
  }
  test("should set correct variable contexts") {
    // GIVEN
    val scoper = new Scoping {
      override def scope(ast: ASTNode, incoming: Scope): ScopingResult = ScopingResult(None, None)
    }

    case object AddContext extends VariableContext
    case object AContext extends VariableContext
    case object BContext extends VariableContext

    val binder = new VariableBinding {
      override def bind(ast: ASTNode, variableContext: VariableContext): VariableContext = ast match {
        case _:Add =>
          variableContext should equal(ReferenceOnly) // the initial context
          AddContext
        case Variable(name) if name == "a" =>
          variableContext should equal(AddContext)
          AContext
        case Variable(name) if name == "b" =>
          variableContext should equal(AddContext)
          BContext

      }
    }

    val checkingBottomUp = new BottomUpVisitor {
      override def visit(ast: ASTNode, variableContext: VariableContext): Unit = ast match {
        case _:Add =>
          variableContext should equal(ReferenceOnly) // the initial context
        case Variable(name) =>
          variableContext should equal(AddContext)

      }
    }
    val statement = Add(varFor("a"), varFor("b"))(pos)

    // WHEN
    new TreeWalker(scoper, binder, mock[TypeExpecting], checkingBottomUp).visit(statement)
  }

  test("should set correct scope") {
    // GIVEN
    val foreachChildScope = new NormalScope().withName("foreachChildScope")
    val withSiblingScope = new NormalScope().withName("withSiblingScope")
    var initialScope: Scope = null

    val scoper = new Scoping {
      override def scope(ast: ASTNode, incoming: Scope): ScopingResult =
        ast match {
          case Property(Variable("head"), _) =>
            initialScope = incoming.asInstanceOf[NormalScope].withName("InitialScope")
            ScopingResult(None, None)

          case Variable("head") =>
            incoming should be theSameInstanceAs initialScope
            ScopingResult(None, None)

          case Property(Variable("with"), _) =>
            incoming should be theSameInstanceAs initialScope
            ScopingResult(None, Some(withSiblingScope))

          case Variable("with") =>
            incoming should be theSameInstanceAs initialScope
            ScopingResult(None, None)

          case Property(Variable("match"), _) =>
            incoming should be theSameInstanceAs withSiblingScope
            ScopingResult(None, None)

          case Variable("match") =>
            incoming should be theSameInstanceAs withSiblingScope
            ScopingResult(None, None)

          case Property(Variable("foreach"), _) =>
            incoming should be theSameInstanceAs withSiblingScope
            ScopingResult(Some(foreachChildScope), Some(incoming))

          case Variable("foreach") =>
            incoming should be theSameInstanceAs foreachChildScope
            ScopingResult(None, None)

          case Property(Variable("last"), _) =>
            incoming should be theSameInstanceAs withSiblingScope
            ScopingResult(None, None)

          case Variable("last") =>
            incoming should be theSameInstanceAs withSiblingScope
            ScopingResult(None, None)

          case _ =>
            ScopingResult(None, None)
        }
    }

    val binder = SemanticTestHelper.mockBinding
    val checkingBottomUp = SemanticTestHelper.mockBottomUpVisitor

    val statement = ListLiteral(Seq(
      prop("head", "noScopeChange"),
      prop("with", "newScopeForSiblings"),
      prop("match", "noScopeChange"),
      prop("foreach", "newScopeForChildren"),
      prop("last", "noScopeChange")
    ))(pos)

    // WHEN
    new TreeWalker(scoper, binder, mock[TypeExpecting], checkingBottomUp).visit(statement)
  }

}
