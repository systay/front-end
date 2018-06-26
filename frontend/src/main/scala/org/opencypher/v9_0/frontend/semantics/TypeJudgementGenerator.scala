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
import org.opencypher.v9_0.expressions._
import org.opencypher.v9_0.frontend.semantics.Types._
import org.opencypher.v9_0.util.attribution.Attribute
import org.opencypher.v9_0.util.{ASTNode, InternalException}

class TypeJudgementGenerator(types: TypeJudgements,
                             bindingsLookup: BindingsLookup,
                             expectations: TypeExpectations) extends BottomUpVisitor {

  private def setTypeFromExpectations(v: LogicalVariable): Unit =
    set(v, expectations.get(v.id))

  override def visit(e: ASTNode): Unit = try {
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

      case x: Subtract => ???
      case x: UnarySubtract => ???
      case x: Multiply => ???
      case x: Divide => ???
      case x: Modulo => ???
      case x: Pow => ???

      // PREDICATES
      case x: Not => ???
      case x: Equals => ???
      case x: Equivalent => ???
      case x: NotEquals => ???
      case x: InvalidNotEquals => ???
      case x: RegexMatch => ???
      case x: And =>
        val lhsT = types.get(x.lhs.id)
        val rhsT = types.get(x.rhs.id)
        set(x, TypeInfo(lhsT.nullable || rhsT.nullable, BoolType))
      case x: Or => ???
      case x: Xor => ???
      case x: Ands => ???
      case x: Ors => ???
      case x: In => ???
      case x: StartsWith => ???
      case x: EndsWith => ???
      case x: Contains => ???
      case x: IsNull => ???
      case x: IsNotNull => ???
      case x: LessThan => ???
      case x: LessThanOrEqual => ???
      case x: GreaterThan => ???
      case x: GreaterThanOrEqual => ???
      case x: PartialPredicate[_] => ???

      //
      case x: CaseExpression => ???
      case x: AndedPropertyInequalities => ???
      case x: CoerceTo => ???
      case x: Property => ???
      case x: FunctionInvocation => ???
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
      case x: CountStar => ???
      case x: PathExpression => ???
      case x: ShortestPathExpression => ???
      case p: PatternExpression =>
        // TODO: We should be able to figure if any of the variables we depend on are nullable, which makes this expression nullable
        types.set(p.id, NullableType(ListType(PathType)))
      case x: IterablePredicateExpression => ???
      case x: ReduceExpression => ???
      case x: ListLiteral =>
        val innerType: Set[NewCypherType] =
          x.expressions.
            map(e => types.get(e.id).possible).
            reduce(_ ++ _)
        set(x, TypeInfo(false, ListType(innerType)))

      case x: ListSlice => ???
      case x: ContainerIndex => ???

      // MAPS
      case x: MapExpression => ???
      case x: MapProjection => ???
      case x: LiteralEntry => ???
      case x: VariableSelector => ???
      case x: PropertySelector => ???
      case x: AllPropertiesSelector => ???
      case x: DesugaredMapProjection => ???

      // LITERALS
      case x: DecimalIntegerLiteral => setNotNullable(x, IntegerType)
      case x: OctalIntegerLiteral => setNotNullable(x, IntegerType)
      case x: HexIntegerLiteral => setNotNullable(x, IntegerType)
      case x: DecimalDoubleLiteral => setNotNullable(x, FloatType)
      case x: StringLiteral => setNotNullable(x, StringType)
      case x: Null => set(x, NullableType(ANY.toSeq: _*))
      case x: BooleanLiteral => setNotNullable(x, BoolType)
      case x: SemanticCheckableExpression => ???
      case _ => ???
    }
  } catch {
    case error: Exception =>
      throw new InternalException(s"Failed when trying to type id: ${e.id} $e \n$error", error)
  }
  private def set(e: Expression, typeInfo: TypeInfo): Unit = {
    types.set(e.id, typeInfo)
  }

  private def setNotNullable(e: Expression, calculatedTypes: NewCypherType*): Unit =
    set(e, new TypeInfo(calculatedTypes.toSet, false))

  private def setNullable(e: Expression, calculatedTypes: NewCypherType*): Unit =
    set(e, new TypeInfo(calculatedTypes.toSet, true))

}

class TypeJudgements extends Attribute[TypeInfo]