/*
* This file is part of the ToolXiT project.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package toolxit
package parser

import util._

import gnieh.pp._

trait TestUtils {

  val env = new RootTeXEnvironment("texput")

  val plainTeXCat = Map(
    '^' -> Category.SUPERSCRIPT,
    '_' -> Category.SUBSCRIPT,
    '{' -> Category.BEGINNING_OF_GROUP,
    '}' -> Category.END_OF_GROUP,
    '#' -> Category.PARAMETER,
    '$' -> Category.MATH_SHIFT,
    '~' -> Category.ACTIVE_CHARACTER
  )

  def withCat(cats: Map[Char, Category.Value])(body: TeXEnvironment => Unit) {
    val env1 = env.enterGroup
    cats foreach {
      case (c, cat) => env1.category(c) = cat
    }
    body(env1)
  }

  val render = new PrettyRenderer(80)

  def print(stream: Stream[Token]) {
    val doc = stream.foldRight(empty) { (token, acc) =>
      token.debug :|: acc
    }
    println(render(group(doc)).layout)
  }

  def pp(token: Token) {
    println(render(token.debug).layout)
  }

  def stringOf(token: Token) =
    token.toString + token.pos

}
