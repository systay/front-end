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
package org.opencypher.v9_0.frontend.phases

import org.opencypher.v9_0.ast.semantics.{SemanticState, SemanticTable}
import org.opencypher.v9_0.ast.{Query, Statement}
import org.opencypher.v9_0.frontend.PlannerName
import org.opencypher.v9_0.frontend.semantics.{TypeExpectations, VariableBindings}
import org.opencypher.v9_0.util.symbols.CypherType
import org.opencypher.v9_0.util.{InputPosition, InputPositions, InternalException}

trait BaseState {
  def queryText: String
  def startPosition: Option[InputPosition]
  def plannerName: PlannerName
  def initialFields: Map[String, CypherType]
  def maybeStatement: Option[Statement]
  def maybeSemantics: Option[SemanticState]
  def maybeExtractedParams: Option[Map[String, Any]]
  def maybeSemanticTable: Option[SemanticTable]
  def maybePositions: Option[InputPositions]
  def maybeBindings: Option[VariableBindings]
  def maybeTypeExpectations: Option[TypeExpectations]

  def accumulatedConditions: Set[Condition]

  def isPeriodicCommit: Boolean = statement() match {
    case Query(Some(_), _) => true
    case _ => false
  }

  def positions(): InputPositions = maybePositions getOrElse fail("InputPositions")
  def statement(): Statement = maybeStatement getOrElse fail("Statement")
  def semantics(): SemanticState = maybeSemantics getOrElse fail("Semantics")
  def extractedParams(): Map[String, Any] = maybeExtractedParams getOrElse fail("Extracted parameters")
  def semanticTable(): SemanticTable = maybeSemanticTable getOrElse fail("Semantic table")

  // SEMANTIC STATE
  def variableBindings(): VariableBindings = maybeBindings getOrElse fail("Variable Bindings")
  def typeExpectations(): TypeExpectations = maybeTypeExpectations getOrElse fail("Type Expectations")


  protected def fail(what: String) = {
    throw new InternalException(s"$what not yet initialised")
  }

  def withStatement(s: Statement): BaseState
  def withSemanticTable(s: SemanticTable): BaseState
  def withSemanticState(s: SemanticState): BaseState
  def withBindings(s: VariableBindings): BaseState
  def withTypeExpectations(s: TypeExpectations): BaseState
  def withParams(p: Map[String, Any]): BaseState
  def withPositions(p: InputPositions): BaseState
}

case class InitialState(queryText: String,
                        startPosition: Option[InputPosition],
                        plannerName: PlannerName,
                        initialFields: Map[String, CypherType] = Map.empty,
                        maybeStatement: Option[Statement] = None,
                        maybePositions: Option[InputPositions] = None,
                        maybeSemantics: Option[SemanticState] = None,
                        maybeExtractedParams: Option[Map[String, Any]] = None,
                        maybeSemanticTable: Option[SemanticTable] = None,
                        maybeBindings: Option[VariableBindings] = None,
                        maybeTypeExpectations: Option[TypeExpectations] = None,
                        accumulatedConditions: Set[Condition] = Set.empty) extends BaseState {

  override def withStatement(s: Statement): InitialState = copy(maybeStatement = Some(s))

  override def withPositions(s: InputPositions): InitialState = copy(maybePositions = Some(s))

  override def withSemanticTable(s: SemanticTable): InitialState = copy(maybeSemanticTable = Some(s))

  override def withSemanticState(s: SemanticState): InitialState = copy(maybeSemantics = Some(s))

  override def withBindings(s: VariableBindings): InitialState = copy(maybeBindings = Some(s))

  override def withTypeExpectations(s: TypeExpectations): InitialState = copy(maybeTypeExpectations = Some(s))

  override def withParams(p: Map[String, Any]): InitialState = copy(maybeExtractedParams = Some(p))
}
