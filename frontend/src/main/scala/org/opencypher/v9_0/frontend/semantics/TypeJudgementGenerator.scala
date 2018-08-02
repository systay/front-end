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

import org.opencypher.v9_0.ast.semantics.SemanticCheckableExpression
import org.opencypher.v9_0.expressions.functions._
import org.opencypher.v9_0.expressions.{functions, _}
import org.opencypher.v9_0.frontend.semantics.Types._
import org.opencypher.v9_0.util.attribution.Attribute
import org.opencypher.v9_0.util.{ASTNode, InternalException}

import scala.collection.mutable

class TypeJudgementGenerator(types: TypeJudgements,
                             bindingsLookup: BindingsLookup,
                             expectations: TypeExpectations) extends BottomUpVisitor {

  override def visit(e: ASTNode, variableContext: VariableContext): Unit = try {
    e match {
      // For a variable declaration, the type is whatever is expected
      case v: LogicalVariable if bindingsLookup.isDeclaration(v) =>
        setTypeFromExpectations(v)

      // Variable references get whatever type the declaration has
      case v: LogicalVariable =>
        val declaration: LogicalVariable = bindingsLookup.declarationOf(v)
        set(v, types.get(declaration.id))

      // ARITHMETICS

      case x: Add =>
        val lhsTypes: TypeInfo = types.get(x.lhs.id)
        val rhsTypes: TypeInfo = types.get(x.rhs.id)
        val scalarTypes: Set[NewCypherType] = new TypeRuleBuilder().
          addRule(StringT, IntegerT, FloatT) (StringT)         (StringT).
          addRule(IntegerT)                  (IntegerT)        (IntegerT).
          addRule(FloatT)                    (FloatT, IntegerT)(FloatT).
          addRule(DurationT)                 (DurationT)       (DurationT).
          addRule(DurationT)                 (DateT)           (DateT).
          addRule(DurationT)                 (TimeT)           (TimeT).
          addRule(DurationT)                 (DateTimeT)       (DateTimeT).
          addRule(DurationT)                 (LocalTimeT)      (LocalTimeT).
          addRule(DurationT)                 (LocalDateT)      (LocalDateT).
          calculate(lhsTypes.possible, rhsTypes.possible)

        /*
        List[A] + List[B] => List[A|B]
        List[A] + B       => List[A|B]
        A + List[B]       => List[A|B]
         */
        val listTypes: Set[NewCypherType] = for {
          lhs <- lhsTypes.possible
          rhs <- rhsTypes.possible if lhs.isList || rhs.isList
        } yield (lhs, rhs) match {
            case (ListT(leftInner), ListT(rightInner)) => ListT(leftInner ++ rightInner)
            case (ListT(leftInner), other) => ListT(leftInner + other)
            case (other, ListT(leftInner)) => ListT(leftInner + other)
            case _ => throw new InternalException("only here to stop warnings")
          }

        val nullability = lhsTypes.nullable || rhsTypes.nullable

        set(x, new TypeInfo(scalarTypes | listTypes, nullability))

      case x: Subtract =>
        val lhsTypes = types.get(x.lhs.id)
        val rhsTypes = types.get(x.rhs.id)

        val possibleTypes = new TypeRuleBuilder().
          addRule(IntegerT)  (IntegerT)           (IntegerT).
          addRule(FloatT)    (FloatT, IntegerT)   (FloatT).
          addRule(DurationT) (DurationT)          (DurationT).
          addRule(DurationT) (DateT)              (DateT).
          addRule(DurationT) (TimeT)              (TimeT).
          addRule(DurationT) (DateTimeT)          (DateTimeT).
          addRule(DurationT) (LocalTimeT)         (LocalTimeT).
          addRule(DurationT) (LocalDateT)         (LocalDateT).
          calculate(lhsTypes.possible, rhsTypes.possible)

        val nullability = lhsTypes.nullable || rhsTypes.nullable
        set(x, new TypeInfo(possibleTypes, nullability))

      case x: UnarySubtract => ???
      case x: Multiply => ???
      case x: Divide => ???
      case x: Modulo => ???
      case x: Pow => ???

      // PREDICATES
      case x: Not => ???
      case x: Equals => binaryBoolean(x)
      case x: Equivalent => binaryBoolean(x)
      case x: NotEquals => binaryBoolean(x)
      case x: InvalidNotEquals => binaryBoolean(x)
      case x: RegexMatch => ???
      case x: And => binaryBoolean(x)
      case x: Or => binaryBoolean(x)
      case x: Xor => binaryBoolean(x)
      case x: Ands => ???
      case x: Ors => ???
      case x: In => ???
      case x: StartsWith => binaryBoolean(x)
      case x: EndsWith => binaryBoolean(x)
      case x: Contains => binaryBoolean(x)
      case x: IsNull => ???
      case x: IsNotNull => ???
      case x: LessThan => binaryBoolean(x)
      case x: LessThanOrEqual => binaryBoolean(x)
      case x: GreaterThan => binaryBoolean(x)
      case x: GreaterThanOrEqual => binaryBoolean(x)
      case x: PartialPredicate[_] => ???

      //
      case x: CaseExpression => ???
      case x: AndedPropertyInequalities => ???
      case x: CoerceTo => ???
      case x: Property =>
        set(x, new TypeInfo(Types.PropertyTypes, nullable = true))
      case x: FunctionInvocation => judgeFunctionInvocation(x)
      case x: GetDegree => ???
      case x: Parameter => ???
      case x: HasLabels => ???

      // ITERABLES
      case x: FilterExpression => ???
      case x: ExtractExpression => ???
      case x: ListComprehension => ???
      case x: PatternComprehension => ???
      case _: FilterScope => ???
      case _: ExtractScope => ???
      case _: ReduceScope => ???
      case x: CountStar =>
        set(x, NonNullableType(IntegerT))
      case x: PathExpression => ???
      case x: ShortestPathExpression => ???
      case p: PatternExpression =>
        // TODO: We should be able to figure if any of the variables we depend on are nullable, which makes this expression nullable
        types.set(p.id, NullableType(ListT(PathT)))
      case x: IterablePredicateExpression => ???
      case x: ReduceExpression => ???
      case x: ListLiteral =>
        val innerType: Set[NewCypherType] =
          x.expressions.
            map(e => types.get(e.id).possible).
            reduce(_ ++ _)
        set(x, TypeInfo(false, ListT(innerType)))

      case x: ListSlice => ???
      case x: ContainerIndex => ???

      // MAPS
      case x: MapExpression =>
        val childrenTypes = x.items.flatMap {
          case (_, child) =>
            types.get(child.id).possible
        }
        setNotNullable(x, MapT(childrenTypes.toSet))
      case x: MapProjection => ???
      case x: LiteralEntry => ???
      case x: VariableSelector => ???
      case x: PropertySelector => ???
      case x: AllPropertiesSelector => ???
      case x: DesugaredMapProjection => ???

      // LITERALS
      case x: DecimalIntegerLiteral => setNotNullable(x, IntegerT)
      case x: OctalIntegerLiteral => setNotNullable(x, IntegerT)
      case x: HexIntegerLiteral => setNotNullable(x, IntegerT)
      case x: DecimalDoubleLiteral => setNotNullable(x, FloatT)
      case x: StringLiteral => setNotNullable(x, StringT)
      case x: Null => set(x, NullableType(ANY.toSeq: _*))
      case x: BooleanLiteral => setNotNullable(x, BoolT)

      case x: SemanticCheckableExpression => ???
      case x: Expression => throw new NotImplementedError(x.toString)
      case _ =>
    }
  } catch {
    case error: Exception =>
      throw new InternalException(s"Failed when trying to type id: ${e.id} $e \n$error", error)
  }

  private def setTypeFromExpectations(v: LogicalVariable): Unit =
    set(v, expectations.get(v.id))

  private def binaryBoolean(x: Expression with BinaryOperatorExpression): Unit = {
    val lhsT = types.get(x.lhs.id)
    val rhsT = types.get(x.rhs.id)
    set(x, TypeInfo(lhsT.nullable || rhsT.nullable, BoolT))
  }

  private def setNotNullable(e: Expression, calculatedTypes: NewCypherType*): Unit =
    set(e, new TypeInfo(calculatedTypes.toSet, false))

  private def set(e: Expression, typeInfo: TypeInfo): Unit = {
    types.set(e.id, typeInfo)
  }

  private def judgeFunctionInvocation(invocation: FunctionInvocation): Unit = invocation.function match {
    case Coalesce =>
      val incomingTypes = invocation.args.map(e => types.get(e.id))
      val nonNullableExpressionFound = incomingTypes.exists(info => !info.nullable)
      val possibleTypes = incomingTypes.map(_.possible).reduce(_ ++ _)
      set(invocation, new TypeInfo(possibleTypes, !nonNullableExpressionFound))

    case _: Function if invocation.args.length <= 1 =>
      val calc: TypeCalc = getTypeCalc(invocation.function)

      val argument: Option[Expression] = invocation.args.headOption
      val in: TypeInfo = argument.map(arg => types(arg.id)).getOrElse(new TypeInfo(Set.empty, nullable = false))

      val result = calc(in.possible)
      set(invocation, new TypeInfo(result, in.nullable))

    case functions.Range =>
      set(invocation, NonNullableType(ListT(IntegerT)))

    case _ => ???
  }

  private def getTypeCalc(func: functions.Function): TypeCalc = func match {
    case Abs =>
      iff(IntegerT -> IntegerT) ||
        iff(FloatT -> FloatT)
    case Acos |
         Asin |
         Atan |
         Atan2 |
         Ceil |
         Cos |
         Cot |
         Degrees |
         Distance |
         E |
         Exp |
         Floor |
         Haversin |
         Log |
         Log10 |
         PercentileCont |
         PercentileDisc |
         Pi |
         Radians |
         Rand |
         Round |
         Sin |
         StdDev |
         StdDevP |
         Sqrt |
         Tan |
         ToFloat =>
      static(FloatT)

    case Avg | Sum | Min | Max =>
      iff(IntegerT -> IntegerT) ||
        iff(FloatT -> FloatT) ||
        iff(DurationT -> DurationT)

    case EndNode |
         StartNode =>
      static(NodeT)

    case Count |
         Id |
         Length |
         Sign |
         Size |
         Timestamp |
         ToInteger =>
      static(IntegerT)

    case Labels |
         Keys =>
      static(ListT(StringT))

    case Left |
         Right |
         LTrim |
         RTrim |
         Replace |
         Trim |
         ToLower |
         ToString |
         ToUpper |
         Type |
         Split |
         Substring =>
      static(StringT)

    case Nodes =>
      static(ListT(NodeT))

    case Point =>
      static(PointT)

    case functions.Range =>
      static(ListT(IntegerT))

    case Relationships =>
      static(ListT(RelationshipT))

    case Reverse =>
      iff(StringT -> StringT) ||
        ifList(sameType)

    case Exists | ToBoolean =>
      static(BoolT)

    case Properties =>
      iff(NodeT -> MapT(Types.PropertyTypes)) ||
        iff(RelationshipT -> MapT(Types.PropertyTypes)) ||
        ifMap(sameType)

    case Head =>
      ifList(in => in.inner)

    case Collect =>
      dynamic(in => ListT(in))

    case Head | Last =>
      ifList(in => in.inner)

    case Reduce =>
      identity // This is only here to give a good error message during linting. fake the type judgement for now
  }


  private def setNullable(e: Expression, calculatedTypes: NewCypherType*): Unit =
    set(e, new TypeInfo(calculatedTypes.toSet, true))

  private def nullInNullOut(invocation: FunctionInvocation, typ: NewCypherType) = {
    val argument = invocation.args.head
    val nullable = types.get(argument.id).nullable
    set(invocation, TypeInfo(nullable, typ))

  }

  case class T(test1: Set[NewCypherType], test2: Set[NewCypherType], result: Set[NewCypherType])

  class TypeRuleBuilder {
    private val acc = new mutable.ListBuffer[(Set[NewCypherType], Set[NewCypherType], Set[NewCypherType])]()

    def addRule(apa1: NewCypherType*)(apa2: NewCypherType*)(apa3: NewCypherType*): TypeRuleBuilder = {
      acc.append((apa1.toSet, apa2.toSet, apa3.toSet))
      this
    }

    def calculate(lhs: Set[NewCypherType], rhs: Set[NewCypherType]): Set[NewCypherType] = acc.toList.foldLeft(Set.empty[NewCypherType]) {
      case (accumulated, (test1, test2, result)) =>
        if (
          overlaps(lhs, test1) && overlaps(rhs, test2) ||
            overlaps(lhs, test2) && overlaps(rhs, test1))
          accumulated ++ result
        else
          accumulated
    }

    private def overlaps(lhs: Set[NewCypherType], rhs: Set[NewCypherType]) = (lhs intersect rhs).nonEmpty
  }

}

class TypeJudgements extends Attribute[TypeInfo]

trait TypeCalc extends (Set[NewCypherType] => Set[NewCypherType]) {
  self =>
  def ||(other: TypeCalc): TypeCalc = new TypeCalc {
    override def apply(in: Set[NewCypherType]): Set[NewCypherType] = {
      val thisResult = self.apply(in)
      val thatResult = other.apply(in)
      thisResult ++ thatResult
    }
  }
}

case object identity extends TypeCalc {
  override def apply(v1: Set[NewCypherType]): Set[NewCypherType] = v1
}

case class dynamic(func: NewCypherType => NewCypherType) extends TypeCalc {
  override def apply(v1: Set[NewCypherType]): Set[NewCypherType] = v1.map(func)
}

case class static(typ: NewCypherType) extends TypeCalc {
  override def apply(in: Set[NewCypherType]): Set[NewCypherType] = Set(typ)
}

case class iff(ifAny: (NewCypherType, NewCypherType)) extends TypeCalc {
  override def apply(in: Set[NewCypherType]): Set[NewCypherType] = in collect {
    case t if t == ifAny._1 => ifAny._2
  }
}

case class ifList(typeCalc: ListT => Set[NewCypherType]) extends TypeCalc {
  override def apply(v1: Set[NewCypherType]): Set[NewCypherType] = {
    v1 collect {
      case t: ListT => typeCalc(t)
    }
  }.flatten
}

case class ifMap(typeCalc: MapT => Set[NewCypherType]) extends TypeCalc {
  override def apply(v1: Set[NewCypherType]): Set[NewCypherType] = {
    v1 collect {
      case t: MapT => typeCalc(t)
    }
  }.flatten
}

object sameType extends (NewCypherType => Set[NewCypherType]) {
  override def apply(v1: NewCypherType): Set[NewCypherType] = Set(v1)
}