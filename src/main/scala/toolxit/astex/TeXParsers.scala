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

import org.parboiled.scala._

/** @author Lucas Satabin
 *
 */
class TeXParser(environment: TeXEnvironment) extends Parser {

  import environment._

  private object ReadingState extends Enumeration {
    // reading state for input reading
    // N = new line
    // M = middle of a line
    // S = skipping blanks
    val N, M, S = Value
  }

  private var state = ReadingState.N

  def tokens: Rule1[List[Token]] = rule {
    zeroOrMore(token) ~~> (_.flatten)
  }

  def token: Rule1[Option[Token]] = rule {
    whitespace ~ (comment ~ push(None) |
      ((controlsequence | parameter ~ run(ReadingState.M) | EOL |
        (character ~~~% { c =>
          if (c.category == Category.SPACE)
            state = ReadingState.S
          else
            state = ReadingState.M
        })) ~~> (t => Some(t))))
  }

  def controlsequence: Rule1[ControlSequenceToken] = rule {
    ignored(ESCAPE_CHARACTER) ~ csname ~~> ControlSequenceToken
  }

  def parameter: Rule1[ParameterToken] = rule {
    ignored(PARAMETER) ~ ("0" - "9") ~> (c => ParameterToken(c.toInt))
  }

  def character: Rule1[CharacterToken] = rule {
    ANY ~~> (c => CharacterToken(c, category(c)))
  }

  def comment: Rule0 = rule {
    COMMENT_CHARACTER ~ zeroOrMore(any ~~? (c => category(c) != Category.END_OF_LINE)) ~ END_OF_LINE ~~% ((_, _) => ())
  }

  def csname: Rule1[String] = rule {
    (oneOrMore(LETTER) ~~> (_.mkString)
      | ANY ~~> (_.toString)) ~ run(state = ReadingState.S)
  }

  // ================ the 16 category code parsers ================

  private def ESCAPE_CHARACTER: Rule1[Char] = rule {
    ANY ~? (c => category(c(0)) == Category.ESCAPE_CHARACTER)
  }

  private def BEGINNING_OF_GROUP: Rule1[Char] = rule {
    ANY ~? (c => category(c(0)) == Category.BEGINNING_OF_GROUP)
  }

  private def END_OF_GROUP: Rule1[Char] = rule {
    ANY ~? (c => category(c(0)) == Category.END_OF_GROUP)
  }

  private def MATH_SHIFT: Rule1[Char] = rule {
    ANY ~? (c => category(c(0)) == Category.MATH_SHIFT)
  }

  private def ALIGNMENT_TAB: Rule1[Char] = rule {
    ANY ~? (c => category(c(0)) == Category.ALIGNMENT_TAB)
  }

  private def END_OF_LINE: Rule1[Char] = rule {
    ANY ~? (c => category(c(0)) == Category.END_OF_LINE)
  }

  private def PARAMETER: Rule1[Char] = rule {
    ANY ~? (c => category(c(0)) == Category.PARAMETER)
  }

  private def RAW_SUPERSCRIPT: Rule1[Char] = rule {
    any ~? (c => category(c(0)) == Category.SUPERSCRIPT)
  }

  private def SUPERSCRIPT: Rule1[Char] = rule {
    ANY ~? (c => category(c(0)) == Category.SUPERSCRIPT)
  }

  private def SUBSCRIPT: Rule1[Char] = rule {
    ANY ~? (c => category(c(0)) == Category.SUBSCRIPT)
  }

  private def IGNORED_CHARACTER: Rule0 = rule {
    ignored(ANY) ~? (c => category(c(0)) == Category.IGNORED_CHARACTER)
  }

  private def SPACE: Rule1[Char] = rule {
    ANY ~? (c => category(c(0)) == Category.SPACE)
  }

  private def LETTER: Rule1[Char] = rule {
    ANY ~? (c => category(c(0)) == Category.LETTER)
  }

  private def OTHER_CHARACTER: Rule1[Char] = rule {
    ANY ~? (c => category(c(0)) == Category.OTHER_CHARACTER)
  }

  private def ACTIVE_CHARACTER: Rule1[Char] = rule {
    ANY ~? (c => category(c(0)) == Category.ACTIVE_CHARACTER)
  }

  private def COMMENT_CHARACTER: Rule1[Char] = rule {
    ANY ~? (c => category(c(0)) == Category.COMMENT_CHARACTER)
  }

  private def INVALID_CHARACTER: Rule1[Char] = rule {
    ANY ~? (c => category(c(0)) == Category.INVALID_CHARACTER)
  }

  // ================ helper parsers ================

  private def EOL: Rule1[Token] = rule {
    END_OF_LINE ~ test(state != ReadingState.S) ~~> { _ =>
      if (state == ReadingState.N) {
        ControlSequenceToken("par")
      } else {
        state = ReadingState.N
        CharacterToken(' ', Category.SPACE)
      }
    }
  }

  private val hexaLower = "0123456789abcdef"

  def any = rule {
    org.parboiled.scala.ANY ~> (c => c(0))
  }

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

  private def whitespace: Rule0 = rule {
    zeroOrMore(SPACE ~~? (_ => state == ReadingState.S || state == ReadingState.N))
  }

  private def ignored(r: => Rule1[_]): Rule0 = rule {
    r ~~% ((_: Any) => ())
  }

}