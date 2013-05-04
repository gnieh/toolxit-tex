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

import scala.annotation.tailrec

/** Utilities methods used by the TeX parsers
 *
 *  @author Lucas Satabin
 */
trait TeXUtils {

  /** Converts an integer to a list of tokens being its decimal representation */
  def toTokens(i: Int): List[Token] = {
    @tailrec
    def aux(i: Int, acc: List[Token]): List[Token] =
      if(i == 0)
        acc
      else
        aux(i / 10, CharacterToken(((i % 10) + 48).toChar, Category.OTHER_CHARACTER) :: acc)
    if(i == 0)
      List(CharacterToken(0, Category.OTHER_CHARACTER))
    else if(i < 0)
      CharacterToken('-', Category.OTHER_CHARACTER) :: aux(i, Nil)
    else
      aux(i, Nil)
  }

  /* substitute parameters by the concrete arguments */
  def substituteParameters(replacement: List[Token], args: List[Token]): List[Token] =
    replacement map {
      case ParameterToken(n) =>
        args(n - 1)
      case GroupToken(open, inner, close) =>
        // replace in depth
        GroupToken(open, substituteParameters(inner, args), close)
      case token =>
        token
    }

  def flattened(tokens: List[Token]): List[Token] = tokens match {
    case GroupToken(open, inner, close) :: tail =>
      open :: flattened(inner) ::: List(close) ::: flattened(tail)
    case token :: tail =>
      token :: flattened(tail)
    case Nil =>
      Nil
  }

}
