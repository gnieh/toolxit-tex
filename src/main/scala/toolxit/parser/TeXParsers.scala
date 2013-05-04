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
abstract class TeXParsers extends Parsers[Token]
                          with NumberParsers
                          with TeXDefinitionParsers
                          with TeXUtils {

  type State = TeXState

  case class TeXState(
    // the rest of the input
    stream: Stream[Token],
    // the current position
    pos: Pos,
    // the current TeX environment with all local and global definitions
    env: TeXEnvironment,
    // when parsing macro parameter text, what is the current parameter number
    currentParam: Int = 0,
    // the current group nesting level
    currentNesting: Int = 0,
    // when parsing shall we expand the control sequences?
    expansion: Boolean = true) extends Input

  protected def makeState(old: State, stream: Stream[Token], pos: Pos): State =
    old.copy(stream = stream, pos = pos)

  /** Parser that parses a command, performing all needed expansions */
  lazy val command: Parser[Command] =
    // TODO implement
    fail("not implemented yet")

  /** Parser that parses and expands the next token */
  lazy val expanded: Parser[Token] =
    // rules for expansion are in the TeX book, starting at page 212
    (for {
      // if this is a control sequence...
      ControlSequenceToken(name) <- any
      // ... that is a macro, ...
      Some(TeXMacro(_, params, repl, long)) <- fromEnv(name)
      // ... parse the parameters (which are not expanded)...
      () <- updateState(st => st.copy(expansion = false))
      args <- paramParser(long, name, params)
      // restore expansion
      () <- updateState(st => st.copy(expansion = true))
      // ... and substitute them in replacement text
      replaced = substituteParameters(repl, args)
      // finally, replace by replacement text...
      () <- updateState { st =>
        val newStream = flattened(replaced).toStream ++ st.stream
        st.copy(stream = newStream)
      }
      // ... and retry
      tok <- expanded
    } yield tok) <|>
    (for {
      // if this is the \number control sequence...
      ControlSequenceToken("number") <- any
      // ... read the following (expanded) number...
      num <- number
      // replace by the decimal representation of the number
      () <- updateState { st =>
        val decimal = toTokens(num)
        val newStream = decimal.toStream ++ st.stream
        st.copy(stream = newStream)
      }
      // ... and retry
      tok <- expanded
    } yield tok) <|>
    // TODO implement me
    any

  def fromEnv(name: String): Parser[Option[ControlSequence]] =
    for {
      st <- getState
    } yield st.env.css(name)

  /** Parser that parses the next expanded token if the expansion process is active, otherwise returns the next raw token */
  lazy val next: Parser[Token] =
    (for {
      st <- getState
      if st.expansion
      t <- expanded
    } yield t) <|> any

  /** Parser that parses a single token, which can be a simple next token or a group token
   *  The groups must be correctly nested */
  lazy val single: Parser[Token] =
    (for {
      open <- beginningOfGroup
      // enter new group in environment
      () <- updateState(st => st.copy(env = st.env.enterGroup))
      tokens <- until(single, endOfGroup)
      close <- endOfGroup
      // leave group
      () <- updateState(st => st.copy(env = st.env.leaveGroup))
    } yield GroupToken(open, tokens, close)) <|>
    param <|>
    next

  /** Parser that accepts the given character token, with same category code */
  def char(c: CharacterToken): Parser[CharacterToken] =
    for {
      (ch @ CharacterToken(value, cat)) <- next
      if value == c.value && cat == c.category
    } yield ch

  /** Parser that accepts the given character token sequence, with same category codes */
  def charSequence(chars: List[CharacterToken]): Parser[Unit] = chars match {
    case c :: rest => char(c) >>= (_ => charSequence(rest))
    case Nil       => success()
  }

  /** Parser that accepts a sequence of 0 or more character tokens */
  lazy val characters: Parser[List[CharacterToken]] =
    many(character)

  /** Parser that accepts any character token */
  lazy val character: Parser[CharacterToken] =
    for {
      (c @ CharacterToken(_, _)) <- next
    } yield c

  /** Parser that accepts any control sequence */
  lazy val controlSequence: Parser[ControlSequenceToken] =
    for {
      (cs @ ControlSequenceToken(_)) <- next
    } yield cs

  /** Parser that accepts any control sequence, without performing any expansion */
  lazy val rawControlSequence: Parser[ControlSequenceToken] =
    for {
      (cs @ ControlSequenceToken(_)) <- any
    } yield cs

  /** Parser that accepts the control sequence with the given name */
  def controlSequence(name: String): Parser[ControlSequenceToken] =
    for {
      cs <- controlSequence
      if cs.name == name
    } yield cs

  lazy val parameter: Parser[CharacterToken] =
    for {
      (c @ CharacterToken(_, Category.PARAMETER)) <- next
    } yield c

  /** Parser that accepts any character of category 'beginning of group' */
  lazy val beginningOfGroup: Parser[CharacterToken] =
    for {
      (c @ CharacterToken(_, cat)) <- next
      if cat == Category.BEGINNING_OF_GROUP
    } yield c

  /** Parser that accepts any character of category 'end of group' */
  lazy val endOfGroup: Parser[CharacterToken] =
    for {
      (c @ CharacterToken(_, cat)) <- next
      if cat == Category.END_OF_GROUP
    } yield c

  lazy val param: Parser[ParameterToken] =
    for {
      _ <- parameter
      nb <- digit
    } yield ParameterToken(nb)

  private lazy val digit =
    for {
      CharacterToken(c, _) <- next
      if c.isDigit
    } yield (c - 48)

  /** Parser that parses the given parameter tokens for macro invocation */
  def paramParser(long: Boolean, name: String, params: List[Parameter]): Parser[List[Token]] = params match {
    case Left(ParameterToken(_)) :: rest if long =>
      // number does not matter here, we know that it is correct
      for {
        // parse the next (single) token
        p <- single
        // then the rest of the parameters
        rest <- paramParser(long, name, rest)
      } yield p :: rest
    case Left(ParameterToken(_)) :: rest =>
      // the long modifier was not given, do not allow \par to occur
      (for {
        // `\par` is not allowed
        () <- not(controlSequence("par"))
        // parse the next (single) token
        p <- single
        // then the rest of the parameters
        rest <- paramParser(long, name, rest)
      } yield p :: rest) <|>
      fail("Paragraph ended before \\$name was complete")
    case Right(chars) :: rest =>
      def sequence(tokens: List[Token]): Parser[Unit] = tokens match {
        case (c @ CharacterToken(_, _)) :: rest =>
          for {
            _ <- char(c)
            () <- sequence(rest)
          } yield ()
        case ControlSequenceToken(name) :: rest =>
          for {
            _ <- controlSequence(name)
            () <- sequence(rest)
          } yield ()
        case token :: rest =>
          fail("Unexpected token " + token)
        case Nil =>
          success()
      }
      for {
        // parse these delimiter characters (and ignore them)
        _ <- sequence(chars)
        // then the rest of the parameters
        rest <- paramParser(long, name, rest)
      } yield rest
    case Nil =>
      success(Nil)
  }

}
