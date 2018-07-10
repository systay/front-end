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

import org.opencypher.v9_0.frontend.semantics.Types.{ListT, MapT, NewCypherType}

object Types {

  val ANY: Set[NewCypherType] = Set(
    ListT.ListOfUnknown,
    MapT.MapOfUnknown,
    IntegerT,
    StringT,
    FloatT,
    BoolT,
    NodeT,
    RelationshipT,
    PointT,
    GeometryT,
    PathT,
    GraphRefT,
    DateT,
    TimeT,
    DateTimeT,
    LocalDateT,
    LocalTimeT,
    DurationT
  )

  val PropertyTypes: Set[NewCypherType] = Set(
    StringT, IntegerT, FloatT, BoolT, PointT, GeometryT, DateT, TimeT, LocalTimeT, DateTimeT, LocalDateT, DurationT
  )

  sealed trait NewCypherType {
    def isList = false

    def isMap = false
  }

  case class ListT(inner: Set[NewCypherType]) extends NewCypherType {
    override def toString: String = s"List[${inner.mkString(",")}]"

    override def isList: Boolean = true
  }

  case class MapT(possibleTypes: Set[NewCypherType]) extends NewCypherType {
    override def toString: String = s"Map[${possibleTypes.mkString(",")}]"

    override def isMap: Boolean = true
  }

  case object IntegerT extends NewCypherType
  case object StringT extends NewCypherType
  case object FloatT extends NewCypherType
  case object BoolT extends NewCypherType
  case object NodeT extends NewCypherType
  case object RelationshipT extends NewCypherType
  case object PointT extends NewCypherType
  case object GeometryT extends NewCypherType
  case object PathT extends NewCypherType
  case object GraphRefT extends NewCypherType
  case object DateT extends NewCypherType
  case object TimeT extends NewCypherType
  case object DateTimeT extends NewCypherType
  case object LocalDateT extends NewCypherType
  case object LocalTimeT extends NewCypherType
  case object DurationT extends NewCypherType


  // This special type is used to stop type possibility explosion -
  // a list of any type could be a list of lists, or a list of list of list.
  // Instead, we model these using List[
  case object ? extends NewCypherType

  object ListT {
    val ListOfUnknown = ListT(?)

    def apply(t: NewCypherType*): ListT = ListT(t.toSet)
  }

  object MapT {
    val MapOfUnknown = MapT(?)

    def apply(t: NewCypherType*): MapT = MapT(t.toSet)
  }

}

// TypeInfo contains both the set of possibles types and whether it is nullable or not.
class TypeInfo(val possible: Set[NewCypherType], val nullable: Boolean) {
  /**
    * Used to check if this TypeInfo, when seen as an expectaion, could work with an expression
    * judged to have the type @judgement.
    *
    * @param judgement The types to check
    * @return
    */
  def isSatisfiedBy(judgement: TypeInfo): Boolean = {
    val acceptableWithCheckingLists = (this.possible intersect judgement.possible).nonEmpty
    if (acceptableWithCheckingLists)
      true
    else {
      val lhs = normalize()
      val rhs = judgement.normalize()

      // If normalization changed anything, let's check with the normalized types
      if (lhs != this || rhs != judgement)
        lhs isSatisfiedBy rhs
      else {
        // As a last ditch - let's check for the ? type
        (possible.contains(ListT.ListOfUnknown) && judgement.possible.exists(_.isList)) ||
        (possible.contains(MapT.MapOfUnknown) && judgement.possible.exists(_.isMap))
      }
    }
  }

  /**
    * Normalization of TypeInfo means that all generic types are turned into single generic type.
    *
    * (List[(Int, String)]) => (List[(Int)], List[(String)]
    *
    * The two are equivalent forms, but the right version is easier to work with.
    *
    * @return
    */
  private def normalize(): TypeInfo = {

    def setMap(in: Set[NewCypherType]): Set[NewCypherType] = in flatMap {
      case ListT(inner) if inner.size > 1 =>
        val innerUnpacked = setMap(inner)
        innerUnpacked.map(i => ListT(Set(i)))
      case ListT(inner) => Some(ListT(setMap(inner)))
      case MapT(inner) if inner.size > 1 =>
        val innerUnpacked = setMap(inner)
        innerUnpacked.map(i => MapT(Set(i)))
      case MapT(inner) => Some(MapT(setMap(inner)))
      case x => Some(x)
    }

    new TypeInfo(setMap(possible), nullable)
  }

  def containsAnyOf(types: NewCypherType*): Boolean = (possible intersect (types.toSet)).nonEmpty

  def containsAny(typ: Set[NewCypherType]): Boolean = (possible intersect typ).nonEmpty

  override def equals(other: Any): Boolean = other match {
    case that: TypeInfo =>
      (that canEqual this) &&
        possible == that.possible &&
        nullable == that.nullable
    case _ => false
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[TypeInfo]

  override def hashCode(): Int = possible.hashCode() + 31 * nullable.hashCode()


  override def toString = s"TypeInfo((${possible.mkString(", ")}), nullable = $nullable)"
}

object TypeInfo {
  def apply(nullable: Boolean, types: NewCypherType*) = new TypeInfo(types.toSet, nullable)
}

object NullableType {
  def apply(possible: NewCypherType*) = new TypeInfo(possible.toSet, nullable = true)
}

object NonNullableType {
  def apply(possible: NewCypherType*) = new TypeInfo(possible.toSet, nullable = false)
}