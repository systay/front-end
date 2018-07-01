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

import org.opencypher.v9_0.frontend.semantics.Types.{ListType, MapType, NewCypherType}

object Types {

  val ANY: Set[NewCypherType] = Set(
    ListType.ListOfUnknown,
    MapType.MapOfUnknown,
    IntegerType,
    StringType,
    FloatType,
    BoolType,
    NodeType,
    RelationshipType,
    PointType,
    GeometryType,
    PathType,
    GraphRefType,
    DateType,
    TimeType,
    DateTimeType,
    LocalDateTimeType,
    LocalTimeType,
    DurationType
  )

  sealed trait NewCypherType {
    def isList = false

    def isMap = false
  }

  case class ListType(inner: Set[NewCypherType]) extends NewCypherType {
    override def toString: String = s"List[${inner.mkString(",")}]"

    /**
      * Instead of grabbing the inner field directly, use this method,
      * that will expand List[?] to any valid inner type
      *
      * @return
      */
    def elementTypes: Set[NewCypherType] =
      if (inner.size == 1 && inner.head == ?)
        ???
      else
        inner

    override def isList: Boolean = true
  }

  case class MapType(possibleTypes: Set[NewCypherType]) extends NewCypherType {
    override def toString: String = s"Map[${possibleTypes.mkString(",")}]"

    override def isMap: Boolean = true
  }

  case object IntegerType extends NewCypherType
  case object StringType extends NewCypherType
  case object FloatType extends NewCypherType
  case object BoolType extends NewCypherType
  case object NodeType extends NewCypherType
  case object RelationshipType extends NewCypherType
  case object PointType extends NewCypherType
  case object GeometryType extends NewCypherType
  case object PathType extends NewCypherType
  case object GraphRefType extends NewCypherType
  case object DateType extends NewCypherType
  case object TimeType extends NewCypherType
  case object DateTimeType extends NewCypherType
  case object LocalDateTimeType extends NewCypherType
  case object LocalTimeType extends NewCypherType
  case object DurationType extends NewCypherType


  // This special type is used to stop type possibility explosion -
  // a list of any type could be a list of lists, or a list of list of list.
  // Instead, we model these using List[
  case object ? extends NewCypherType

  object ListType {
    val ListOfUnknown = ListType(?)

    def apply(t: NewCypherType*): ListType = ListType(t.toSet)
  }

  object MapType {
    val MapOfUnknown = MapType(?)

    def apply(t: NewCypherType*): MapType = MapType(t.toSet)
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
        (possible.contains(ListType.ListOfUnknown) && judgement.possible.exists(_.isList)) ||
        (possible.contains(MapType.MapOfUnknown) && judgement.possible.exists(_.isMap))
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
      case ListType(inner) if inner.size > 1 =>
        val innerUnpacked = setMap(inner)
        innerUnpacked.map(i => ListType(Set(i)))
      case ListType(inner) => Some(ListType(setMap(inner)))
      case MapType(inner) if inner.size > 1 =>
        val innerUnpacked = setMap(inner)
        innerUnpacked.map(i => MapType(Set(i)))
      case MapType(inner) => Some(MapType(setMap(inner)))
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