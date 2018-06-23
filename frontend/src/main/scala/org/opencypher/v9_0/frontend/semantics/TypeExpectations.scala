package org.opencypher.v9_0.frontend.semantics

import org.opencypher.v9_0.ast.Where
import org.opencypher.v9_0.expressions._
import org.opencypher.v9_0.frontend.semantics.Types._
import org.opencypher.v9_0.util.ASTNode
import org.opencypher.v9_0.util.attribution.Attribute

class TypeExpectationsGenerator(typeExpectations: TypeExpectations) extends TypeExpecting {
  private val bool =
    NullableType(BoolType)
  private val noExpectations =
    NullableType(ANY.toSeq: _*)

  private val numbers =
    NullableType(IntegerType, FloatType)

  private val mappable =
    NullableType(NodeType, RelationshipType, DateType, TimeType, MapType.MapOfUnknown)

  override def visit(ast: ASTNode): Unit = {
    ast match {
      case NodePattern(Some(variable), _, _, _) =>
        set(variable, NullableType(NodeType))

      case RelationshipPattern(Some(variable), _, _, _, _, _ ,_) =>
        set(variable, NullableType(RelationshipType))

      case NamedPatternPart(variable, _) =>
        set(variable, NullableType(PathType))

      case where: Where =>
        set(where.expression, bool)

      // Expressions
      case x: Add =>
        val typeInfo = NullableType(ListType.ListOfUnknown, IntegerType, StringType, FloatType, DateType, TimeType)
        set(x.lhs, typeInfo)
        set(x.rhs, typeInfo)

      case x: BinaryOperatorExpression
        if x.isInstanceOf[Subtract] &&
          x.isInstanceOf[Multiply] &&
          x.isInstanceOf[Divide] &&
          x.isInstanceOf[Pow] &&
          x.isInstanceOf[Modulo] =>
        set(x.lhs, numbers)
        set(x.rhs, numbers)

      case x: UnarySubtract =>
        set(x.rhs, numbers)


      // PREDICATES
      case x: Not =>
        set(x.rhs, bool)

      case x: BinaryOperatorExpression
        if x.isInstanceOf[Equals] &&
          x.isInstanceOf[NotEquals] &&
          x.isInstanceOf[InvalidNotEquals] =>
        set(x.lhs, noExpectations)
        set(x.rhs, noExpectations)


      case x: Equivalent => ???

      case x: BinaryOperatorExpression
        if x.isInstanceOf[And] &&
          x.isInstanceOf[Or] &&
          x.isInstanceOf[Xor] =>
        set(x.lhs, bool)
        set(x.rhs, bool)

      // ANDs and ORs
      case x: MultiOperatorExpression =>
        set(x.exprs, bool)

      case x: In =>
        set(x.lhs, noExpectations)
        set(x.lhs, NullableType(ListType.ListOfUnknown))

      case x: BinaryOperatorExpression
        if x.isInstanceOf[And] &&
          x.isInstanceOf[Or] &&
          x.isInstanceOf[Xor] =>
        set(x.lhs, bool)
        set(x.rhs, bool)


      case x: BinaryOperatorExpression
        if x.isInstanceOf[StartsWith] &&
          x.isInstanceOf[EndsWith] &&
          x.isInstanceOf[RegexMatch] &&
          x.isInstanceOf[Contains] =>
        set(x.lhs, NullableType(StringType))
        set(x.rhs, NullableType(StringType))

      // isNULL and isNOTNULL
      case x: RightUnaryOperatorExpression =>
        set(x, noExpectations)

      // > or < or <= or >=
      case x: InequalityExpression =>
        set(x.lhs, NullableType(IntegerType, StringType, FloatType, DateType, TimeType, BoolType))
        set(x.rhs, NullableType(IntegerType, StringType, FloatType, DateType, TimeType, BoolType))

      case x: CaseExpression if x.expression.nonEmpty =>
        x.alternatives.foreach { case (condition, result) =>
          set(condition, bool)
          set(result, noExpectations)
        }

      case x: CaseExpression =>
        x.alternatives.foreach { case (condition, result) =>
          set(condition, noExpectations)
          set(result, noExpectations)
        }

      case x: CoerceTo =>
        set(x.expr, noExpectations)

      case x: FunctionInvocation =>
        set(x.args, noExpectations)

      case x: GetDegree =>
        set(x.node, NullableType(NodeType))

      case x: HasLabels =>
        set(x.expression, NullableType(NodeType))

      // ITERABLES
      case x: FilterExpression =>
        set(x.expression, NullableType(ListType.ListOfUnknown))
        set(x.scope.innerPredicate, bool)

      case x: ExtractExpression =>
        set(x.expression, NullableType(ListType.ListOfUnknown))
        set(x.scope.innerPredicate, bool)

      case x: ListComprehension =>
        set(x.expression, NullableType(ListType.ListOfUnknown))
        set(x.scope.innerPredicate, bool)

      case x: IterablePredicateExpression =>
        set(x.expression, NullableType(ListType.ListOfUnknown))
        set(x.scope.innerPredicate, bool)

      case x: PathExpression => ???

      case x: ReduceExpression =>
        set(x.list, NullableType(ListType.ListOfUnknown))

      case x: ListLiteral =>
        set(x.expressions, noExpectations)

      case x: ListSlice =>
        set(x.list, NullableType(ListType.ListOfUnknown))
        set(x.from, NonNullableType(IntegerType))
        set(x.to, NonNullableType(IntegerType))

      case x: ContainerIndex =>
        set(x.expr, mappable)
        set(x.idx, NonNullableType(StringType))

      // MAPS

      case x: MapExpression =>
        set(x.items.map(_._2), noExpectations)

      case x: MapProjection =>
        set(x.name, mappable)

      case x: LiteralEntry =>
        set(x.exp, noExpectations)

      case x: VariableSelector => ???
      case x: PropertySelector => ???

      case x: DesugaredMapProjection =>
        set(x.name, mappable)

      case property: Property =>
        set(property.map, mappable)

      case _ =>
    }
  }

  def set(ast: ASTNode, t: TypeInfo): Unit = typeExpectations.set(ast.id, t)
  def set(ast: Traversable[ASTNode], t: TypeInfo): Unit = ast.foreach(e => typeExpectations.set(e.id, t))
}

class TypeExpectations extends Attribute[TypeInfo]
