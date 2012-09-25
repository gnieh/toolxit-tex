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

/** A TeX parser provides a way to parse a TeX input and return a list of tokens
 *  expanded as needed.
 *
 *  @author Lucas Satabin
 *
 */
trait TeXParser extends Parser {

  /** The TeX environment determining how to parse tokens */
  val environment: TeXEnvironment

  def tokens: Rule1[List[Token]]

  def token: Rule1[Token]

  def controlsequence: Rule1[ControlSequenceToken]

  def parameter: Rule1[ParameterToken]

  def character: Rule1[CharacterToken]

  def comment: Rule0

  def csname: Rule1[String]

  def ESCAPE_CHARACTER: Rule1[Char]

  def BEGINNING_OF_GROUP: Rule1[Char]

  def END_OF_GROUP: Rule1[Char]

  def MATH_SHIFT: Rule1[Char]

  def ALIGNMENT_TAB: Rule1[Char]

  def END_OF_LINE: Rule1[Char]

  def PARAMETER: Rule1[Char]

  def RAW_SUPERSCRIPT: Rule1[Char]

  def SUPERSCRIPT: Rule1[Char]

  def SUBSCRIPT: Rule1[Char]

  def IGNORED_CHARACTER: Rule0

  def SPACE: Rule1[Char]

  def LETTER: Rule1[Char]

  def OTHER_CHARACTER: Rule1[Char]

  def ACTIVE_CHARACTER: Rule1[Char]

  def COMMENT_CHARACTER: Rule1[Char]

  def INVALID_CHARACTER: Rule1[Char]

  def ANY: Rule1[Char]

  def parameterText: Rule1[List[Parameter]]

  def group: Rule1[List[Token]]

  def argumentParser(parameters: List[Parameter]): Rule1[List[List[Token]]]

}