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
package org.opencypher.v9_0.parser

import org.opencypher.v9_0.ast
import org.opencypher.v9_0.util.attribution.{Attributes, IdGen}
import org.opencypher.v9_0.util.{InputPosition, InputPositions, InternalException, SyntaxException}
import org.parboiled.scala._

/**
  * This class should not be used to parse multiple queries. It should only be used once.
  */
class CypherParser(attributes: Attributes) extends Parser
  with Statement
  with Expressions {

  override implicit val idGen: IdGen = attributes.idGen
  override protected val positions = new InputPositions

  // TODO: This is a hack. If we can't re-use the parser, we should design it differently.
  private var _used = false

  @throws(classOf[SyntaxException])
  def parse(queryText: String, offset: Option[InputPosition] = None): ParseResult =
    if (_used)
      throw new InternalException("not safe to use the parser for more than one query!")
    else {
      _used = true
      ParseResult(parseOrThrow(queryText, offset, statements), positions)
    }

  private val statements: Rule1[Seq[ast.Statement]] = rule {
    oneOrMore(WS ~ Statement ~ WS, separator = ch(';')) ~~ optional(ch(';')) ~~ EOI.label("end of input")
  }

}

case class ParseResult(statement: ast.Statement, positions: InputPositions)