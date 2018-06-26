package org.opencypher.v9_0.frontend.semantics

import org.opencypher.v9_0.ast.{AliasedReturnItem, Where}
import org.opencypher.v9_0.expressions._
import org.opencypher.v9_0.frontend.semantics.Types._
import org.opencypher.v9_0.util.ASTNode
import org.opencypher.v9_0.util.attribution.{Attribute, Id}

import scala.collection.mutable

/**
  * This class is responsible for setting expectations on expressions. Expectations are expressed as TypeInfos.
  * They come from either expressions containing sub-expressions or from clauses that contain expressions.
  *
  * Type expectations are generated when going down the tree. Unfortunately, in some situations,
  * we can't calculate expectations on children until at least some of their type judgements
  * have been calculated. The most blatant example is `+` which can only figure out expectations after
  * both children have been typed.
  *
  * This is solved by having very vague expectations in this phase and
  * then update them when we come back up the tree again, as part of type judgements.
  *
  * @param typeExpectations This is the Attribute which expectations are written to.
  * @param types This Attribute is used to look up the types om
  */
class TypeExpectationsGenerator(typeExpectations: TypeExpectations, types: TypeJudgements) extends TypeExpecting {
  private val bool =
    NullableType(BoolType)

  private val noExpectations =
    NullableType(ANY.toSeq: _*)

  private val numbers =
    NullableType(IntegerType, FloatType)

  private val mappable =
    NullableType(NodeType, RelationshipType, DateType, TimeType, MapType.MapOfUnknown)

  // When going down the tree, we might know that one of our children is expected
  // to have the same type as a different child. Since children have been yet to be
  // typed, we save the information here, so that when we get to the child, we can ask
  // its sibling what type it has.
  private val typeExpectFromReference = new mutable.HashMap[Id, Id]()
  private def copyTypesBetween(from: ASTNode, to: ASTNode): Unit = {
    if(types.contains(from.id))
      typeExpectations.set(to.id, types.get(from.id))
    else {
      println(s"waiting to set expectation on ${to.id} until we know the type of ${from.id}")
      typeExpectFromReference.put(from.id, to.id)
    }
  }
  private def set(ast: ASTNode, t: TypeInfo): Unit = typeExpectations.set(ast.id, t)
  private def set(ast: Traversable[ASTNode], t: TypeInfo): Unit = ast.foreach(e => typeExpectations.set(e.id, t))


  override def visit(ast: ASTNode, bindingMode: BindingMode): Unit = {
    val nullable = bindingMode match {
      case BindingAllowed(x) => x
      case _                 => false
    }
    ast match {
      case v: LogicalVariable if typeExpectFromReference.contains(v.id) =>
        set(v, types.get(typeExpectFromReference(v.id)))

      case NodePattern(Some(variable), _, _, _) =>
        set(variable, TypeInfo(nullable, NodeType))

      case RelationshipPattern(Some(variable), _, _, _, _, _ ,_) =>
        set(variable, TypeInfo(nullable, RelationshipType))

      case NamedPatternPart(variable, _) =>
        set(variable, TypeInfo(nullable, PathType))

      case where: Where =>
        set(where.expression, bool)

      case AliasedReturnItem(exp, alias) =>
        copyTypesBetween(from = exp, to = alias)

      // Expressions
      case x: Add =>
        // For `+`, we have to redo type expectations when we know the type of lhs
        val typeInfo = NullableType(ListType.ListOfUnknown, IntegerType, StringType, FloatType, DateType, TimeType)
        set(x.lhs, typeInfo)
        set(x.rhs, typeInfo)

        // - * / ^ MOD
      case x: BinaryOperatorExpression
        if x.isInstanceOf[Subtract] ||
           x.isInstanceOf[Multiply] ||
           x.isInstanceOf[Divide] ||
           x.isInstanceOf[Pow] ||
           x.isInstanceOf[Modulo] =>
        set(x.lhs, numbers)
        set(x.rhs, numbers)

      case x: UnarySubtract =>
        set(x.rhs, numbers)

      // PREDICATES
      case x: Not =>
        set(x.rhs, bool)

      case x: BinaryOperatorExpression
        if x.isInstanceOf[Equals] ||
           x.isInstanceOf[NotEquals] ||
           x.isInstanceOf[InvalidNotEquals] =>
        set(x.lhs, noExpectations)
        set(x.rhs, noExpectations)

      case x: Equivalent => ???

      case x: BinaryOperatorExpression
        if x.isInstanceOf[And] ||
           x.isInstanceOf[Or] ||
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
        if x.isInstanceOf[And] ||
           x.isInstanceOf[Or] ||
           x.isInstanceOf[Xor] =>
        set(x.lhs, bool)
        set(x.rhs, bool)


      case x: BinaryOperatorExpression
        if x.isInstanceOf[StartsWith] ||
           x.isInstanceOf[EndsWith] ||
           x.isInstanceOf[RegexMatch] ||
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
}

class TypeExpectationsAfterTyping(typeExpectations: TypeExpectations, typeJudgements: TypeJudgements) extends BottomUpVisitor {

  val temporalTypes: Set[NewCypherType] = Set(DateType, TimeType, DateTimeType, LocalDateTimeType, LocalTimeType)

  override def visit(e: ASTNode): Unit = e match {
    case Add(lhs, rhs) =>
      val lhsType: TypeInfo = typeJudgements.get(lhs.id)

      // Strings
      // "a" + "b" => "ab"
      // "a" + 1 => "a1"
      // "a" + 1.1 => "a1.1"
      // Numbers
      // 1 + "b" => "1b"
      // 1 + 1 => 2
      // 1 + 1.1 => 2.1
      // 1.1 + "b" => "1.1b"
      // 1.1 + 1 => 2.1
      // 1.1 + 1.1 => 2.2
      // Temporals
      // T + Duration => T
      // Duration + T => T
      // Duration + Duration => Duration
      val valueTypes: Set[NewCypherType] =
        if (lhsType containsAnyOf(StringType, IntegerType, FloatType))
          Set(StringType, IntegerType, FloatType)
        else
          Set.empty

      val temporalExpectations =
        if (lhsType containsAny temporalTypes)
          Set(DurationType)
        else
          Set.empty

      val durationTypes =
        if (lhsType containsAnyOf DurationType)
          temporalTypes
        else
          Set.empty

      // [a] + [b] => [a, b]
      // [a] + b => [a, b]
      // a + [b] => [a, b]
      val listTypes = Set(ListType.ListOfUnknown)

      val totalExpectations = valueTypes ++ temporalExpectations ++ durationTypes ++ listTypes

      typeExpectations.set(rhs.id, new TypeInfo(possible = totalExpectations, nullable = true) )

    case _ =>
  }

  private def when(e: Expression) = new TypeCalculation(e)

  class TypeCalculation(lhs: Expression) {
    val lhsTypeInfo = typeJudgements.get(lhs.id)
    def contains(typ: NewCypherType) = this
    def ||(typ: NewCypherType*) = this
    def andThen = this
  }
}

// Here we store all the known expectations on expressions. This could be expectations coming from a clause,
// or from a parent expression. This information is used to check if the query can run at all, and then to see
// where we need to insert coercions and null-checks
class TypeExpectations extends Attribute[TypeInfo]
