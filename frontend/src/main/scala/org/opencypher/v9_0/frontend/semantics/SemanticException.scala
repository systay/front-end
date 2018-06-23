package org.opencypher.v9_0.frontend.semantics

import org.opencypher.v9_0.expressions.LogicalVariable
import org.opencypher.v9_0.util.spi.MapToPublicExceptions
import org.opencypher.v9_0.util.{ASTNode, CypherException}

abstract class SemanticException(message: String, ast: ASTNode) extends CypherException(message) {
  override def mapToPublic[T <: Throwable](mapper: MapToPublicExceptions[T]): T = mapper.invalidSemanticException(message, null)
}

class VariableNotDeclaredError(variable: LogicalVariable) extends SemanticException(s"Variable not declared `${variable.name}`", variable)
class VariableAlreadyDeclaredInScopeException(variable: LogicalVariable) extends SemanticException(s"Variable already declared in scope `${variable.name}`", variable)