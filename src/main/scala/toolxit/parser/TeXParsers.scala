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

/** A bunch of parsers that transform and expand the TeX tokens. The results
 *  are primitive TeX commands. The parsers must perform macro expansion when needed.
 *
 *  @author Lucas Satabin
 */
abstract class TeXParsers extends Parsers[Token] {

  type State = TeXState

  case class TeXState(stream: Stream[Token], pos: Pos, env: TeXEnvironment) extends Input

  protected def makeState(old: State, stream: Stream[Token], pos: Pos): State =
    old.copy(stream = stream, pos = pos)

  /** Parser that parses a command, performing all needed expansions */
  lazy val command: Parser[Command] =
    // TODO implement
    fail("not implemented yet")

  /** Parser that parses the next expanded token */
  lazy val expanded: Parser[Token] =
    // TODO implement
    fail("not implemented yet")

  /** Parser that accepts the given character token, with same category code */
  def char(c: CharacterToken): Parser[CharacterToken] =
    for {
      (ch @ CharacterToken(value, cat)) <- expanded
      if value == c.value && cat == c.category
    } yield ch

  /** Parser that accepts the given character token sequence, with same category codes */
  def charSequence(chars: List[CharacterToken]): Parser[Unit] = chars match {
    case c :: rest => char(c) >>= (_ => charSequence(rest))
    case Nil       => success()
  }

  /** Parser that accepts any control sequence */
  lazy val controlSequence: Parser[ControlSequenceToken] =
    for {
      (cs @ ControlSequenceToken(_)) <- expanded
    } yield cs

  /** Parser that accepts the control sequence with the given name */
  def controlSequence(name: String): Parser[ControlSequenceToken] =
    for {
      cs <- controlSequence
      if cs.name == name
    } yield cs

  /** Parser that accepts any character of category 'beginning of group' */
  lazy val beginningOfGroup: Parser[CharacterToken] =
    for {
      (c @ CharacterToken(_, cat)) <- expanded
      if cat == Category.BEGINNING_OF_GROUP
    } yield c

  /** Parser that accepts any character of category 'end of group' */
  lazy val endOfGroup: Parser[CharacterToken] =
    for {
      (c @ CharacterToken(_, cat)) <- expanded
      if cat == Category.END_OF_GROUP
    } yield c

  lazy val param: Parser[ParameterToken] =
    for {
      (p @ ParameterToken(_)) <- expanded
    } yield p

  /** Parser that parses the given parameter tokens for macro invocation */
  def paramParser(params: List[Parameter]): Parser[List[Token]] = params match {
    case Left(ParameterToken(_)) :: rest =>
      // number does not matter here, we know that it is correct
      for {
        // parse the next (expanded) token
        p <- expanded
        // then the rest of the parameters
        rest <- paramParser(rest)
      } yield p :: rest
    case Right(chars) :: rest =>
      for {
        // parse these delimiter characters (and ignore them)
        _ <- charSequence(chars)
        // then the rest of the parameters
        rest <- paramParser(rest)
      } yield rest
    case Nil =>
      success(Nil)
  }

}
