package org.opencypher.v9_0.frontend.semantics

import org.opencypher.v9_0.ast._
import org.opencypher.v9_0.expressions._
import org.opencypher.v9_0.parser.CypherParser
import org.opencypher.v9_0.util.attribution.{Attributes, SequentialIdGen}
import org.opencypher.v9_0.util.test_helpers.CypherFunSuite

class ScopingTest extends CypherFunSuite {


  test("WITH creates sibling scopes") {
    val parser = new CypherParser(Attributes(new SequentialIdGen()))
    val x = parser.parse("MATCH (a) WITH a.prop as x RETURN *")
    val scopes = new Scopes

    new TreeWalker(Scoper, scopes).visit(x.statement)

    val matchNode = x.statement.findByClass[Match]
    val returnNode = x.statement.findByClass[Return]

    val matchScope = scopes.get(matchNode.id)
    val returnScope = scopes.get(returnNode.id)

    matchScope should not equal returnScope
    matchScope.popScope() should equal(returnScope.popScope())
  }

  test("ORDER BY lives in a special scope") {
    val parser = new CypherParser(Attributes(new SequentialIdGen()))

    val x = parser.parse("MATCH (a) WITH a ORDER BY a.prop RETURN *")
    val scopes = new Scopes

    new TreeWalker(Scoper, scopes).visit(x.statement)

    val orderBy = x.statement.findByClass[OrderBy]
    val orderByScope = scopes.get(orderBy.id)

    orderByScope shouldBe a[BiScope]

    val matchNode = x.statement.findByClass[Match]
    val returnNode = x.statement.findByClass[Return]

    val matchScope = scopes.get(matchNode.id)
    val returnScope = scopes.get(returnNode.id)

    val biScope = orderByScope.asInstanceOf[BiScope]
    biScope.firstScope should equal(returnScope)
    biScope.secondScope should equal(matchScope)
  }

  test("FOREACH creates inner scopes for the updates") {
    val parser = new CypherParser(Attributes(new SequentialIdGen()))
    val x = parser.parse("CREATE (a) FOREACH(x in [1,2,3] | SET a.prop = x) REMOVE a.prop")
    val scopes = new Scopes

    new TreeWalker(Scoper, scopes).visit(x.statement)

    val create = x.statement.findByClass[Create]
    val set = x.statement.findByClass[SetClause]
    val literalList = x.statement.findByClass[ListLiteral]
    val foreach = x.statement.findByClass[Foreach]
    val delete = x.statement.findByClass[Remove]

    val createScope = scopes.get(create.id)
    val setScope = scopes.get(set.id)
    val listScope = scopes.get(literalList.id)
    val foreachScope = scopes.get(foreach.id)
    val deleteScope = scopes.get(delete.id)

    createScope should equal(foreachScope)
    createScope should equal(deleteScope)
    setScope.popScope() should equal(createScope)
    listScope should equal(createScope)
  }

  test("ListComprehension") {
    val parser = new CypherParser(Attributes(new SequentialIdGen()))
    val x = parser.parse("RETURN [ x in $param | x + 12 ]")
    val scopes = new Scopes

    new TreeWalker(Scoper, scopes).visit(x.statement)

    val returnClass = x.statement.findByClass[Return]
    val listComp = x.statement.findByClass[ListComprehension]
    val add = x.statement.findByClass[Add]

    val returnClassScope = scopes.get(returnClass.id)
    val listCompScope = scopes.get(listComp.id)
    val addScope = scopes.get(add.id)

    returnClassScope should equal(listCompScope)
    addScope.popScope() should equal(listCompScope)
  }

  test("PatternComprehension") {
    val parser = new CypherParser(Attributes(new SequentialIdGen()))
    val x = parser.parse("MATCH (a: Person) RETURN [ (a)-[:FRIEND]->(b) | b.name ]")
    val scopes = new Scopes

    new TreeWalker(Scoper, scopes).visit(x.statement)

    val returnClass = x.statement.findByClass[Return]
    val pattComp = x.statement.findByClass[PatternComprehension]
    val relPattern = x.statement.findByClass[RelationshipsPattern]
    val projection = x.statement.findByClass[Property]

    val returnClassScope = scopes.get(returnClass.id)
    val pattCompScope = scopes.get(pattComp.id)
    val projectionScope = scopes.get(projection.id)
    val relPatternScope = scopes.get(relPattern.id)

    projectionScope should equal(relPatternScope)
    relPatternScope.popScope() should equal(pattCompScope)
  }

  test("UNION") {
    val parser = new CypherParser(Attributes(new SequentialIdGen()))
    val x = parser.parse("RETURN 1 as X UNION RETURN '1' AS X")
    val scopes = new Scopes

    new TreeWalker(Scoper, scopes).visit(x.statement)

    val integer = x.statement.findByClass[SignedIntegerLiteral]
    val string = x.statement.findByClass[StringLiteral]

    val intScope = scopes.get(integer.id)
    val strScope = scopes.get(string.id)

    intScope should not equal(strScope)
    intScope.popScope() should equal(strScope.popScope())
  }

}
