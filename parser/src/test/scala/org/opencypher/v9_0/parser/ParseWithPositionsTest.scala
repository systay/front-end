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

import org.opencypher.v9_0.expressions.{Add, DecimalDoubleLiteral, SignedIntegerLiteral}
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
import org.opencypher.v9_0.util.InputPosition
import org.opencypher.v9_0.util.attribution.{Attributes, SequentialIdGen}
import org.opencypher.v9_0.util.test_helpers.CypherFunSuite

class ParseWithPositionsTest extends CypherFunSuite {
  test("we are able to see positions") {
    import org.opencypher.v9_0.util.Foldable._
    val parser = new CypherParser(Attributes(new SequentialIdGen()))
           //01234567890123456
    val q = "RETURN 1.2 + 2"
    val result = parser.parse(q, None)
    val positions = result.positions
    val ast = result.statement
    positions.get(ast.findByClass[Add].id) should equal(InputPosition(14, 1, 15))
    positions.get(ast.findByClass[SignedIntegerLiteral].id) should equal(InputPosition(13, 1, 14))
    positions.get(ast.findByClass[DecimalDoubleLiteral].id) should equal(InputPosition(7, 1, 8))
  }
}
