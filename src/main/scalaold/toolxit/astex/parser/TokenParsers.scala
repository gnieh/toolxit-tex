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

/** Provides parsers to return an (unexpanded) token stream form parsed characters.
 *
 *  @author Lucas Satabin
 *
 */
abstract class TokenParsers extends CharacterParsers {

  import environment._

  def tokens: Rule1[List[Token]] = rule {
    zeroOrMore(token)
  }

  def token: Rule1[Token] = rule {
    // ignore whitespace and comments
    zeroOrMore(whitespace | comment) ~
      (controlsequence | parameter ~ run(ReadingState.M) | EOL |
        (character ~~~% { c =>
          if (c.category == Category.SPACE)
            state = ReadingState.S
          else
            state = ReadingState.M
        }))
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

}