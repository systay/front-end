package org.opencypher.v9_0.frontend.semantics

import org.opencypher.v9_0.expressions.Expression
import org.opencypher.v9_0.util.ASTNode

/**
  * This class checks whether type expectations and judgements align well enough to so that executing the query makes sense.
  * Type errors can still occur, but that would be runtime checks and not something we can detect at this stage.
  */
class TypeChecker(typeExpectations: TypeExpectations, typeJudgements: TypeJudgements) extends BottomUpVisitor {
  override def visit(node: ASTNode): Unit = node match {
    case e: Expression =>
      val expected: TypeInfo = typeExpectations.get(e.id)
      val judged: TypeInfo = typeJudgements.get(e.id)
      val overlap = expected.possible intersect judged.possible

      if(overlap.isEmpty) {
        // Uh-oh... Let's check if coercions are possible
        throw new TypeExpectationsNotMetException(expected, judged, e)
      }


    case _ =>
  }
}
