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
package org.opencypher.v9_0.util

import org.opencypher.v9_0.util.Rewritable._
import org.opencypher.v9_0.util.attribution.{Id, IdGen}

trait ASTNode extends Product with Foldable with Rewritable {

  self =>

  val idGen: IdGen

  val id: Id = idGen.id()

  // FIXME this is a workaround due to a scala bug (https://github.com/scala/bug/issues/10667, should be removed when the scala bug is fixed
  // TODO remove also when constructors have only 2 argument sets again (after removing input position)
  def selfThis: this.type = this

  def position: InputPosition

  def dup(children: Seq[AnyRef]): this.type =
    if (children.iterator eqElements this.children)
      this
    else {
      val constructor = Rewritable.copyConstructor(this)
      val params = constructor.getParameterTypes
      val args = children.toVector
      val size = params.length
      val ctorArgs =
        if (
        size == args.length + 2 && // Has the normal constructor arguments plus two extra
          params(size - 2).isAssignableFrom(classOf[InputPosition]) && // The first of those extra is an InputPosition
          params(size - 1).isAssignableFrom(classOf[IdGen])) // And the second one is an IdGen
        args :+ this.position :+ this.idGen
      else if (
        size == args.length + 1 && // Has the normal constructor arguments plus one extra
          params(size - 1).isAssignableFrom(classOf[IdGen])) // And the extra is an IdGen
        args :+ this.idGen
      else
        args

      val duped = constructor.invoke(this, ctorArgs: _*)
      duped.asInstanceOf[self.type]
    }

  def asCanonicalStringVal: String = toString
}
