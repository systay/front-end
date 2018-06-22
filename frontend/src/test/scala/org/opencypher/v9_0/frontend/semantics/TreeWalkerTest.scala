package org.opencypher.v9_0.frontend.semantics

import org.opencypher.v9_0.ast._
import org.opencypher.v9_0.expressions._
import org.opencypher.v9_0.parser.CypherParser
import org.opencypher.v9_0.util.ASTNode
import org.opencypher.v9_0.util.attribution.{Attributes, SequentialIdGen}
import org.opencypher.v9_0.util.test_helpers.CypherFunSuite

class TreeWalkerTest extends CypherFunSuite {
  test("should visit things in the correct order") {
    var seenNodes = Seq[Class[_]]()
    val scoping = new Scoping {
      override def scope(ast: ASTNode, incoming: Scope, scopes: Scopes): ScopingResult = {
        seenNodes = seenNodes :+ ast.getClass
        ScopingResult(None, None)
      }
    }

    val parser = new CypherParser(Attributes(new SequentialIdGen()))
    val x = parser.parse("MATCH (a) WITH a.prop as x ORDER BY a.foo, x RETURN *")
    new TreeWalker(scoping, new Scopes).visit(x.statement)


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
}
