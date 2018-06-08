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
package org.opencypher.v9_0.rewriting.rewriters

import org.opencypher.v9_0.expressions._
import org.opencypher.v9_0.util
import org.opencypher.v9_0.util.attribution.Attributes
import org.opencypher.v9_0.util.{Rewriter, bottomUp}

case class foldConstants(attributes: Attributes) extends Rewriter {
  def apply(that: AnyRef): AnyRef =
  try {
    instance.apply(that)
  } catch {
    case e: java.lang.ArithmeticException => throw new util.ArithmeticException(e.getMessage, e)
  }
  private val instance: Rewriter = bottomUp(Rewriter.lift {
    case e@Add(lhs: SignedIntegerLiteral, rhs: SignedIntegerLiteral) =>
      SignedDecimalIntegerLiteral((lhs.value + rhs.value).toString)(e.position)(attributes.copy(e.id))
    case e@Add(lhs: DecimalDoubleLiteral, rhs: SignedIntegerLiteral) =>
      DecimalDoubleLiteral((lhs.value + rhs.value).toString)(e.position)(attributes.copy(e.id))
    case e@Add(lhs: SignedIntegerLiteral, rhs: DecimalDoubleLiteral) =>
      DecimalDoubleLiteral((lhs.value + rhs.value).toString)(e.position)(attributes.copy(e.id))
    case e@Add(lhs: DecimalDoubleLiteral, rhs: DecimalDoubleLiteral) =>
      DecimalDoubleLiteral((lhs.value + rhs.value).toString)(e.position)(attributes.copy(e.id))

    case e@Subtract(lhs: SignedIntegerLiteral, rhs: SignedIntegerLiteral) =>
      SignedDecimalIntegerLiteral((lhs.value - rhs.value).toString)(e.position)(attributes.copy(e.id))
    case e@Subtract(lhs: DecimalDoubleLiteral, rhs: SignedIntegerLiteral) =>
      DecimalDoubleLiteral((lhs.value - rhs.value).toString)(e.position)(attributes.copy(e.id))
    case e@Subtract(lhs: SignedIntegerLiteral, rhs: DecimalDoubleLiteral) =>
      DecimalDoubleLiteral((lhs.value - rhs.value).toString)(e.position)(attributes.copy(e.id))
    case e@Subtract(lhs: DecimalDoubleLiteral, rhs: DecimalDoubleLiteral) =>
      DecimalDoubleLiteral((lhs.value - rhs.value).toString)(e.position)(attributes.copy(e.id))

    case e@Multiply(lhs: SignedIntegerLiteral, rhs: SignedIntegerLiteral) =>
      SignedDecimalIntegerLiteral((lhs.value * rhs.value).toString)(e.position)(attributes.copy(e.id))
    case e@Multiply(lhs: DecimalDoubleLiteral, rhs: SignedIntegerLiteral) =>
      DecimalDoubleLiteral((lhs.value * rhs.value).toString)(e.position)(attributes.copy(e.id))
    case e@Multiply(lhs: SignedIntegerLiteral, rhs: DecimalDoubleLiteral) =>
      DecimalDoubleLiteral((lhs.value * rhs.value).toString)(e.position)(attributes.copy(e.id))
    case e@Multiply(lhs: DecimalDoubleLiteral, rhs: DecimalDoubleLiteral) =>
      DecimalDoubleLiteral((lhs.value * rhs.value).toString)(e.position)(attributes.copy(e.id))

    case e@Multiply(lhs: NumberLiteral, rhs: NumberLiteral) =>
      e.selfThis
    case e@Multiply(lhs: NumberLiteral, rhs) =>
      Multiply(rhs, lhs)(e.position)(attributes.copy(e.id)).rewrite(instance)
    case e@Multiply(lhs@Multiply(innerLhs, innerRhs: NumberLiteral), rhs: NumberLiteral) =>
      val multiply = Multiply(innerRhs, rhs)(lhs.position)(attributes.copy(lhs.id))
      Multiply(multiply, innerLhs)(e.position)(attributes.copy(e.id)).rewrite(instance)
    case e@Multiply(lhs@Multiply(innerLhs: NumberLiteral, innerRhs), rhs: NumberLiteral) =>
      Multiply(Multiply(innerLhs, rhs)(lhs.position)(attributes.copy(lhs.id)), innerRhs)(e.position)(attributes.copy(e.id)).rewrite(instance)

    case e@Divide(lhs: SignedIntegerLiteral, rhs: SignedIntegerLiteral) =>
      SignedDecimalIntegerLiteral((lhs.value / rhs.value).toString)(e.position)(attributes.copy(e.id))
    case e@Divide(lhs: DecimalDoubleLiteral, rhs: SignedIntegerLiteral) =>
      DecimalDoubleLiteral((lhs.value / rhs.value).toString)(e.position)(attributes.copy(e.id))
    case e@Divide(lhs: SignedIntegerLiteral, rhs: DecimalDoubleLiteral) =>
      DecimalDoubleLiteral((lhs.value / rhs.value).toString)(e.position)(attributes.copy(e.id))
    case e@Divide(lhs: DecimalDoubleLiteral, rhs: DecimalDoubleLiteral) =>
      DecimalDoubleLiteral((lhs.value / rhs.value).toString)(e.position)(attributes.copy(e.id))

    case e@Modulo(lhs: SignedIntegerLiteral, rhs: SignedIntegerLiteral) =>
      SignedDecimalIntegerLiteral((lhs.value % rhs.value).toString)(e.position)(attributes.copy(e.id))
    case e@Modulo(lhs: DecimalDoubleLiteral, rhs: SignedIntegerLiteral) =>
      DecimalDoubleLiteral((lhs.value % rhs.value).toString)(e.position)(attributes.copy(e.id))
    case e@Modulo(lhs: SignedIntegerLiteral, rhs: DecimalDoubleLiteral) =>
      DecimalDoubleLiteral((lhs.value % rhs.value).toString)(e.position)(attributes.copy(e.id))
    case e@Modulo(lhs: DecimalDoubleLiteral, rhs: DecimalDoubleLiteral) =>
      DecimalDoubleLiteral((lhs.value % rhs.value).toString)(e.position)(attributes.copy(e.id))

    case e@Pow(lhs: SignedIntegerLiteral, rhs: SignedIntegerLiteral) =>
      DecimalDoubleLiteral(Math.pow(lhs.value.toDouble, rhs.value.toDouble).toString)(e.position)(attributes.copy(e.id))
    case e@Pow(lhs: DecimalDoubleLiteral, rhs: SignedIntegerLiteral) =>
      DecimalDoubleLiteral(Math.pow(lhs.value, rhs.value.toDouble).toString)(e.position)(attributes.copy(e.id))
    case e@Pow(lhs: SignedIntegerLiteral, rhs: DecimalDoubleLiteral) =>
      DecimalDoubleLiteral(Math.pow(lhs.value.toDouble, rhs.value).toString)(e.position)(attributes.copy(e.id))
    case e@Pow(lhs: DecimalDoubleLiteral, rhs: DecimalDoubleLiteral) =>
      DecimalDoubleLiteral(Math.pow(lhs.value, rhs.value).toString)(e.position)(attributes.copy(e.id))

    case e: UnaryAdd =>
      e.rhs

    case e@UnarySubtract(rhs: SignedIntegerLiteral) =>
      SignedDecimalIntegerLiteral((-rhs.value).toString)(e.position)(attributes.copy(e.id))
    case e: UnarySubtract =>
      Subtract(SignedDecimalIntegerLiteral("0")(e.position)(attributes.copy(e.id)), e.rhs)(e.position)(attributes.copy(e.id))

    case e@Equals(lhs: IntegerLiteral, rhs: IntegerLiteral) => asAst(lhs.value == rhs.value, e.selfThis)
    case e@Equals(lhs: DoubleLiteral, rhs: DoubleLiteral) => asAst(lhs.value == rhs.value, e.selfThis)
    case e@Equals(lhs: IntegerLiteral, rhs: DoubleLiteral) => asAst(lhs.value.doubleValue() == rhs.value, e.selfThis)
    case e@Equals(lhs: DoubleLiteral, rhs: IntegerLiteral) => asAst(lhs.value == rhs.value.doubleValue(), e.selfThis)

    case e@LessThan(lhs: IntegerLiteral, rhs: IntegerLiteral) => asAst(lhs.value < rhs.value, e.selfThis)
    case e@LessThan(lhs: DoubleLiteral, rhs: DoubleLiteral) => asAst(lhs.value < rhs.value, e.selfThis)
    case e@LessThan(lhs: IntegerLiteral, rhs: DoubleLiteral) => asAst(lhs.value.doubleValue() < rhs.value, e.selfThis)
    case e@LessThan(lhs: DoubleLiteral, rhs: IntegerLiteral) => asAst(lhs.value < rhs.value.doubleValue(), e.selfThis)

    case e@GreaterThan(lhs: IntegerLiteral, rhs: IntegerLiteral) => asAst(lhs.value > rhs.value, e.selfThis)
    case e@GreaterThan(lhs: DoubleLiteral, rhs: DoubleLiteral) => asAst(lhs.value > rhs.value, e.selfThis)
    case e@GreaterThan(lhs: IntegerLiteral, rhs: DoubleLiteral) => asAst(lhs.value.doubleValue() > rhs.value, e.selfThis)
    case e@GreaterThan(lhs: DoubleLiteral, rhs: IntegerLiteral) => asAst(lhs.value > rhs.value.doubleValue(), e.selfThis)
  })

  private def asAst(b: Boolean, e: Expression) = if (b) True()(e.position)(attributes.copy(e.id)) else False()(e.position)(attributes.copy(e.id))
}
