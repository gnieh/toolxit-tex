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
package toolxit.astex
package parser

import org.parboiled.scala._

/** Provides parsers for basic characters that may then be transformed to tokens.
 *
 *  @author Lucas Satabin
 *
 */
abstract class CharacterParsers extends TeXParser {

  import environment._

  protected object ReadingState extends Enumeration {
    // reading state for input reading
    // N = new line
    // M = middle of a line
    // S = skipping blanks
    val N, M, S = Value
  }

  protected var state = ReadingState.N

  def ESCAPE_CHARACTER: Rule1[Char] = rule {
    ANY ~? (c => category(c(0)) == Category.ESCAPE_CHARACTER)
  }

  def BEGINNING_OF_GROUP: Rule1[Char] = rule {
    ANY ~? (c => category(c(0)) == Category.BEGINNING_OF_GROUP)
  }

  def END_OF_GROUP: Rule1[Char] = rule {
    ANY ~? (c => category(c(0)) == Category.END_OF_GROUP)
  }

  def MATH_SHIFT: Rule1[Char] = rule {
    ANY ~? (c => category(c(0)) == Category.MATH_SHIFT)
  }

  def ALIGNMENT_TAB: Rule1[Char] = rule {
    ANY ~? (c => category(c(0)) == Category.ALIGNMENT_TAB)
  }

  def END_OF_LINE: Rule1[Char] = rule {
    ANY ~? (c => category(c(0)) == Category.END_OF_LINE)
  }

  def PARAMETER: Rule1[Char] = rule {
    ANY ~? (c => category(c(0)) == Category.PARAMETER)
  }

  def RAW_SUPERSCRIPT: Rule1[Char] = rule {
    any ~? (c => category(c(0)) == Category.SUPERSCRIPT)
  }

  def SUPERSCRIPT: Rule1[Char] = rule {
    ANY ~? (c => category(c(0)) == Category.SUPERSCRIPT)
  }

  def SUBSCRIPT: Rule1[Char] = rule {
    ANY ~? (c => category(c(0)) == Category.SUBSCRIPT)
  }

  def IGNORED_CHARACTER: Rule0 = rule {
    ignored(ANY) ~? (c => category(c(0)) == Category.IGNORED_CHARACTER)
  }

  def SPACE: Rule1[Char] = rule {
    ANY ~? (c => category(c(0)) == Category.SPACE)
  }

  def LETTER: Rule1[Char] = rule {
    ANY ~? (c => category(c(0)) == Category.LETTER)
  }

  def OTHER_CHARACTER: Rule1[Char] = rule {
    ANY ~? (c => category(c(0)) == Category.OTHER_CHARACTER)
  }

  def ACTIVE_CHARACTER: Rule1[Char] = rule {
    ANY ~? (c => category(c(0)) == Category.ACTIVE_CHARACTER)
  }

  def COMMENT_CHARACTER: Rule1[Char] = rule {
    ANY ~? (c => category(c(0)) == Category.COMMENT_CHARACTER)
  }

  def INVALID_CHARACTER: Rule1[Char] = rule {
    ANY ~? (c => category(c(0)) == Category.INVALID_CHARACTER)
  }

  private val hexaLower = "0123456789abcdef"

  def ANY: Rule1[Char] = rule {
    (RAW_SUPERSCRIPT ~ RAW_SUPERSCRIPT ~~? ((sup1, sup2) => sup1 == sup2) ~
      any ~ any
      ~~~? ((h1, h2) => hexaLower.contains(h1) && hexaLower.contains(h2)) ~~> {
        (h1: Char, h2: Char) =>
          // next character of the form `^^XX` where X is one of `0123456789abcdef`
          (h1 << 4 + h2).toChar
      }) |
      (RAW_SUPERSCRIPT ~ RAW_SUPERSCRIPT ~~? ((sup1, sup2) => sup1 == sup2) ~
        any ~~~? (_ < 128) ~~> { (letter: Char) =>
          // next character of the form `^^A`
          if (letter < 64) {
            (letter + 64).toChar
          } else {
            (letter - 64).toChar
          }
        }) |
        any
  }

  protected def any = rule {
    org.parboiled.scala.ANY ~> (c => c(0))
  }

  protected def whitespace: Rule0 = rule {
    oneOrMore(SPACE ~~? (_ => state == ReadingState.S || state == ReadingState.N))
  }

  protected def ignored(r: Rule1[_]): Rule0 = rule {
    r ~~% ((_: Any) => ())
  }

  protected def EOL: Rule1[Token] = rule {
    END_OF_LINE ~ test(state != ReadingState.S) ~~> { _ =>
      if (state == ReadingState.N) {
        ControlSequenceToken("par")
      } else {
        state = ReadingState.N
        CharacterToken(' ', Category.SPACE)
      }
    }
  }

}