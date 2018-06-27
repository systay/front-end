package org.opencypher.v9_0.frontend.semantics

import org.opencypher.v9_0.util.ASTNode
import org.opencypher.v9_0.util.Foldable._

object TreeWithId2String {
  def toString(root: ASTNode): String = {
    val builder = new StringBuilder

    def add(node: AnyRef, indent: Int): Unit = {
      val newIndent =
        node match {
          case ast: ASTNode =>
            builder.append(" " * indent + ast.getClass.getSimpleName + "#" + ast.id.x + System.lineSeparator())
            indent + 2
          case _ =>
            indent
        }

      node.children.foreach { child =>
        add(child, newIndent)
      }
    }

    add(root, 0)

    builder.toString()
  }
}
