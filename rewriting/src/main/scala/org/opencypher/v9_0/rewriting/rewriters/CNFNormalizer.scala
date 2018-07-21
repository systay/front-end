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
import org.opencypher.v9_0.expressions.functions.Exists
import org.opencypher.v9_0.rewriting.AstRewritingMonitor
import org.opencypher.v9_0.util.Foldable._
import org.opencypher.v9_0.util.attribution.{Attributes, SameId}
import org.opencypher.v9_0.util.helpers.fixedPoint
import org.opencypher.v9_0.util.{Rewriter, bottomUp, inSequence, topDown}


case class deMorganRewriter(attributes: Attributes)(implicit monitor: AstRewritingMonitor) extends Rewriter {

  override def apply(that: AnyRef): AnyRef = instance(that)

  private val step = Rewriter.lift {
    case p@Xor(expr1, expr2) =>
      val copyVariableRewriter = copyVariables(attributes)
      val innerAnd = And(expr1.endoRewrite(copyVariableRewriter), expr2.endoRewrite(copyVariableRewriter))(p.position)(attributes.copy(p.id))
      val not = Not(innerAnd)(p.position)(attributes.copy(p.id))
      val or = Or(expr1, expr2)(p.position)(attributes.copy(p.id))
      And(or, not)(p.position)(attributes.copy(p.id))
    case p@Not(And(exp1, exp2)) =>
      Or(Not(exp1)(p.position)(attributes.copy(p.id)), Not(exp2)(p.position)(attributes.copy(p.id)))(p.position)(attributes.copy(p.id))
    case p@Not(Or(exp1, exp2)) =>
      And(Not(exp1)(p.position)(attributes.copy(p.id)), Not(exp2)(p.position)(attributes.copy(p.id)))(p.position)(attributes.copy(p.id))
  }

  private val instance: Rewriter = repeatWithSizeLimit(bottomUp(step))(monitor)
}

object distributeLawsRewriter {
  // converting from DNF to CNF is exponentially expensive, so we only do it for a small amount of clauses
  // see https://en.wikipedia.org/wiki/Conjunctive_normal_form#Conversion_into_CNF
  val DNF_CONVERSION_LIMIT = 8
}

case class distributeLawsRewriter(attributes: Attributes)(implicit monitor: AstRewritingMonitor) extends Rewriter {
  def apply(that: AnyRef): AnyRef = {
    if (dnfCounts(that) < distributeLawsRewriter.DNF_CONVERSION_LIMIT)
      instance(that)
    else {
      monitor.abortedRewritingDueToLargeDNF(that)
      that
    }
  }

  private def dnfCounts(value: Any) = value.treeFold(1) {
    case Or(lhs, a: And) => acc => (acc + 1, Some(identity))
    case Or(a: And, rhs) => acc => (acc + 1, Some(identity))
  }

  private val step = Rewriter.lift {
    case p@Or(exp1, And(exp2, exp3)) =>
      And(
        Or(exp1, exp2)(p.position)(attributes.copy(p.id)),
        Or(exp1.endoRewrite(copyVariables(attributes)), exp3)(p.position)(attributes.copy(p.id)))(p.position)(attributes.copy(p.id))
    case p@Or(And(exp1, exp2), exp3) =>
      And(
        Or(exp1, exp3)(p.position)(attributes.copy(p.id)),
        Or(exp2, exp3.endoRewrite(copyVariables(attributes)))(p.position)(attributes.copy(p.id)))(p.position)(attributes.copy(p.id))
  }

  private val instance: Rewriter = repeatWithSizeLimit(bottomUp(step))(monitor)
}

object flattenBooleanOperators extends Rewriter {
  def apply(that: AnyRef): AnyRef = instance.apply(that)

  private val firstStep: Rewriter = Rewriter.lift {
    case p@And(lhs, rhs) => Ands(Set(lhs, rhs))(p.position)(SameId(p.id))
    case p@Or(lhs, rhs)  => Ors(Set(lhs, rhs))(p.position)(SameId(p.id))
  }

  private val secondStep: Rewriter = Rewriter.lift {
    case p@Ands(exprs) => Ands(exprs.flatMap {
      case Ands(inner) => inner
      case x => Set(x)
    })(p.position)(SameId(p.id))
    case p@Ors(exprs) => Ors(exprs.flatMap {
      case Ors(inner) => inner
      case x => Set(x)
    })(p.position)(SameId(p.id))
  }

  private val instance = inSequence(bottomUp(firstStep), fixedPoint(bottomUp(secondStep)))
}

case class simplifyPredicates(attributes: Attributes) extends Rewriter {
  def apply(that: AnyRef): AnyRef = instance.apply(that)

  private val step: Rewriter = Rewriter.lift {
    case Not(Not(exp))                                     => exp
    case p@Ands(exps) if exps.exists(_.isInstanceOf[True]) =>
      val expressions = exps.filterNot(_.isInstanceOf[True])
      if(expressions.isEmpty)
        True()(p.position)(attributes.copy(p.id))
      else
        Ands(expressions)(p.position)(SameId(p.id))
    case p@Ors(exps) if exps.exists(_.isInstanceOf[False]) =>
      val expressions = exps.filterNot(_.isInstanceOf[False])
      if(expressions.isEmpty)
        False()(p.position)(attributes.copy(p.id))
      else
        Ors(expressions)(p.position)(SameId(p.id))
    case p@Ors(exps) if exps.exists(_.isInstanceOf[True])  => True()(p.position)(attributes.copy(p.id))
    case p@Ands(exps) if exps.exists(_.isInstanceOf[False])=> False()(p.position)(attributes.copy(p.id))
  }


  private val instance = fixedPoint(bottomUp(step))
}

case object normalizeSargablePredicates extends Rewriter {

  override def apply(that: AnyRef): AnyRef = instance(that)

  private val instance: Rewriter = topDown(Rewriter.lift {

    // turn n.prop IS NOT NULL into exists(n.prop)
    case predicate@IsNotNull(_:Property) =>
      Exists.asInvocation(predicate.lhs)(predicate.position, SameId(predicate.id))

    // remove not from inequality expressions by negating them
    case Not(inequality: InequalityExpression) =>
      inequality.negated
  })
}
