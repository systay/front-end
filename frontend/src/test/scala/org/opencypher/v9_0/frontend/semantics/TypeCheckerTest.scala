package org.opencypher.v9_0.frontend.semantics

import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.when
import org.opencypher.v9_0.ast.{AstConstructionTestSupport, Where}
import org.opencypher.v9_0.expressions.{And, True}
import org.opencypher.v9_0.util.test_helpers.CypherFunSuite

class TypeCheckerTest extends CypherFunSuite with AstConstructionTestSupport {
  test("type check fails") {
    // true + 3 will never work
    val bool = True()(pos)
    val three = literalInt(3)
    val and = And(bool, three)(pos)
    val where = Where(and)(pos)
    val judgements = new TypeJudgements
    val expectations = new TypeExpectations
    val expector = new TypeExpectationsGenerator(expectations, judgements)
    val typer = new TypeJudgementGenerator(judgements, mock[BindingsLookup], expectations)
    val typeChecker = new TypeChecker(expectations, judgements)
    val binder = mock[VariableBinder]
    when(binder.bind(any(), any())).thenReturn(BindingAllowed(false))
    val scoping = mock[Scoping]
    when(scoping.scope(any(), any())).thenReturn(ScopingResult(None, None))
    val walker = new TreeWalker(scoping, binder, expector, typer andThen typeChecker)
    intercept[TypeExpectationsNotMetException](walker.visit(where))
  }
}
