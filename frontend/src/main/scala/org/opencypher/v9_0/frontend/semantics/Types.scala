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

import org.opencypher.v9_0.frontend.semantics.Types.NewCypherType

object Types {

  sealed trait NewCypherType

  case class ListType(inner: Set[NewCypherType]) extends NewCypherType {
    override def toString: String = s"List[${inner.mkString(",")}]"

    /**
      * Instead of grabbing the inner field directly, use this method,
      * that will expand List[?] to any valid inner type
      * @return
      */
    def elementTypes: Set[NewCypherType] =
      if (inner.size == 1 && inner.head == ?)
        ???
      else
        inner
  }

  case class MapType(possibleTypes: Set[NewCypherType]) extends NewCypherType {
    override def toString: String = s"Map[${possibleTypes.mkString(",")}]"
  }

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
    TimeType
  )

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
case class TypeInfo(possible: Set[NewCypherType], nullable: Boolean)
object NullableType {
  def apply(possible: NewCypherType*) = TypeInfo(possible.toSet, nullable = true)
}

object NonNullableType {
  def apply(possible: NewCypherType*) = TypeInfo(possible.toSet, nullable = false)
}