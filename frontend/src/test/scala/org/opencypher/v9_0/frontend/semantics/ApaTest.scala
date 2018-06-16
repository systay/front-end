package org.opencypher.v9_0.frontend.semantics

import org.opencypher.v9_0.parser.CypherParser
import org.opencypher.v9_0.util.ASTNode
import org.opencypher.v9_0.util.attribution.{Attributes, SequentialIdGen}
import org.opencypher.v9_0.util.test_helpers.CypherFunSuite

class ApaTest extends CypherFunSuite {
  test("apa") {
    val parser = new CypherParser(Attributes(new SequentialIdGen()))
    val x = parser.parse("MATCH (a) WITH a.prop AS x ORDER BY a.foo, b MATCH (b) RETURN a, x")
    val ast = x.statement
    val scoping = new Scoping
    scoping.gogogo(ast)

    ast.treeExists {
      case x: ASTNode =>
        val read = if (scoping.readScope.contains(x.id))
          " -- R: " + scoping.readScope.get(x.id).toString
        else
          ""
        val write = if (scoping.writeScope.contains(x.id))
          " -- W: " + scoping.writeScope.get(x.id).toString
        else
          ""

        println(s"${x.id.x} -- ${x.getClass.getSimpleName}$read$write")
        false
    }

    VariableBinding.doIt(ast, scoping.readScope, scoping.writeScope)
  }
}
