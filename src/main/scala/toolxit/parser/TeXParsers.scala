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
                          with DimenParsers
                          with FontParsers
                          with TeXDefinitionParsers
                          with IfParsers
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
    expansion: Boolean = true,
    // when set to true, the next \else part encountered will be skipped
    // until the matching \fi
    skipElse: Boolean = false,
    // when the current input is included by another one, refer to the parent input
    including: Option[State] = None,
    // when \endinput was seen, the next EOL or EOI restores the including input
    endinput: Boolean = false) extends Input

  protected def makeState(old: State, stream: Stream[Token], pos: Pos): State =
    old.copy(stream = stream, pos = pos)

  /** Resolves the given name for \input command */
  protected def resolve(name: String): Option[Stream[Token]]

  /** Parser that parses a command, performing all needed expansions */
  lazy val command: Parser[Command] =
    // TODO implement
    fail("not implemented yet")

  /** Parser that parses and expands the next token */
  lazy val expanded: Parser[Token] =
    // rules for expansion are in the TeX book, starting at page 212
    expandedMacro <||>
    expandedInput <||>
    expandedNumber <||>
    expandedRomanNumeral <||>
    expandedString <||>
    expandedJobname <||>
    expandedFontname <||>
    expandedMeaning <||>
    expandedCsname <||>
    expandedExpandafter <||>
    expandedNoexpand <||>
    expandEndinput <||>
    any

  lazy val expandedMacro: Parser[Token] =
    for {
      // if this is a control sequence...
      ControlSequenceToken(name, _) <- any
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
    } yield tok

  lazy val expandedNumber: Parser[Token] =
    for {
      // if this is the \number control sequence...
      ControlSequenceToken("number", false) <- any
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
    } yield tok

  lazy val expandedRomanNumeral: Parser[Token] =
    for {
      // if this is the \romannumeral control sequence...
      ControlSequenceToken("romannumeral", false) <- any
      // ... read the following (expanded) number...
      num <- number
      // replace by the roman representation of the number
      () <- updateState { st =>
        val roman = toRoman(num)
        val newStream = roman.toStream ++ st.stream
        st.copy(stream = newStream)
      }
      // ... and retry
      tok <- expanded
    } yield tok

  lazy val expandedString: Parser[Token] =
    for {
      // if this is the \string control sequence...
      ControlSequenceToken("string", false) <- any
      // read the following (non expanded) token
      tok <- any
      // replace by the string representation of this token
      () <- updateState { st =>
        val toks = toTokens(st.env.toString(tok))
        val newStream = toks.toStream ++ st.stream
        st.copy(stream = newStream)
      }
      // ... and retry
      tok <- expanded
    } yield tok

  lazy val expandedJobname: Parser[Token] =
    for {
      // if this is the \jobname control sequence...
      ControlSequenceToken("jobname", false) <- any
      // ... simply print the control sequence corresponding to environment's job name...
      () <- updateState { st =>
        val toks = toTokens(st.env.jobname)
        val newStream = toks.toStream ++ st.stream
        st.copy(stream = newStream)
      }
      // ... and retry
      tok <- expanded
    } yield tok

  lazy val expandedFontname: Parser[Token] =
    for {
      // if this is the \fontname control sequence...
      ControlSequenceToken("fontname", false) <- any
      // parse the subsequent font...
      f <- font
      // ..., expand its name to a token list...
      () <- updateState { st =>
        val toks = toTokens(f.name + " at size " + f.atSize)
        val newStream = toks.toStream ++ st.stream
        st.copy(stream = newStream)
      }
      // ... and retry
      tok <- expanded
    } yield tok

  lazy val expandedMeaning: Parser[Token] =
    for {
      // if this is the \meaning control sequence...
      ControlSequenceToken("meaning", false) <- any
      // ... get the next unexpanded token...
      tok <- any
      // ... and expand to its meaning...
      () <- updateState { st =>
        val toks = toTokens(st.env.meaning(tok))
        val newStream = toks.toStream ++ st.stream
        st.copy(stream = newStream)
      }
      // ... and retry
      tok <- expanded
    } yield tok

  lazy val expandedCsname: Parser[Token] =
    for {
      // if this is \csname ...
      ControlSequenceToken("csname", false) <- any
      // ... expand tokens until \endcsname...
      tokens <- until(expanded, controlSequence("endcsname"))
      ControlSequenceToken("endcsname", false) <- any
      // ... put tokens on top of the stream...
      () <- updateState { st =>
        val name = st.env.toString(tokens)
        // the corresponding control sequence
        val cs = st.env.css(name) match {
          case Some(_) =>
            ControlSequenceToken(name)
          case None =>
            ControlSequenceToken("relax")
        }
        val newStream = cs #:: st.stream
        st.copy(stream = newStream)
      }
      // ... and retry
      tok <- expanded
    } yield tok

  lazy val expandedExpandafter: Parser[Token] =
    for {
      // if this is \expandafter...
      ControlSequenceToken("expandafter", false) <- any
      // ... read the next unexpanded token...
      next <- any
      // ... expand the next token...
      after <- expanded
      // put the unexpanded token in front...
      () <- updateState { st =>
        st.copy(stream = next #:: after #:: st.stream)
      }
      // ... and retry
      tok <- expanded
    } yield tok

  lazy val expandedNoexpand: Parser[Token] =
    for {
      // if this is \noexpand...
      ControlSequenceToken("noexpand", false) <- any
      // ... get the next unexpanded token
      next <- any
      // update it in the input stream
      () <- updateState { st =>
        // if this should be expanded, treat the token as \relax
        val tok = if(st.env.expandable(next))
          ControlSequenceToken("relax")
        else
          next
        st.copy(stream = tok #:: st.stream)
      }
      // ... and retry
      tok <- expanded
    } yield tok

  lazy val expandedInput: Parser[Token] =
    for {
      // if this is \input...
      ControlSequenceToken("input", false) <- any
      // read until next white space, this is the filename
      name <- until(expanded, whitespace)
      // replace the input by the new resolved stream if it can be resolved
      st <- getState
      resolved = resolve(st.env.toString(name))
      if resolved.isDefined
      () <- setState {
        val input = resolved.get
        st.copy(
          // append \endinput at the end to cause the parser to properly close it
          // when reaching the end. it does not matter if there were previous occurrences
          // of \endinput in the included stream, this one will simply be ignored in this case.
          // the implementation of `++` on stream ensures that the `input`
          // stream is not completely evaluated here
          stream = input ++ Stream(ControlSequenceToken("endinput")),
          pos = StreamPosition(input, 0),
          including = Some(st)
        )
      }
      // ... and retry
      tok <- expanded
    } yield tok

  lazy val expandEndinput: Parser[Token] =
    for {
      // if this is \endinput
      ControlSequenceToken("endinput", false) <- any
      // input ends on next end of line character (or at EOI)
      () <- updateState { st =>
        st.copy(endinput = true)
      }
      // ... and retry
      tok <- expanded
    } yield tok

  def fromEnv(name: String): Parser[Option[ControlSequence]] =
    for {
      st <- getState
    } yield st.env.css(name)

  /** Parser that parses the next expanded token if the expansion process is active, otherwise returns the next raw token */
  lazy val next: Parser[Token] = {
    val inner = attempt(for {
        st <- getState
        if st.expansion
        t <- expanded
      } yield t) <||>
      any

    for {
      // end of line read
      CharacterToken(_, Category.END_OF_LINE) <- inner
      // and endinput flag set
      st <- getState
      if st.endinput
      // restore including input if any or set empty stream
      () <- setState {
        st.including match {
          case Some(state) => state.copy(env = st.env)
          case None        => makeState(st, Stream.empty, StreamPosition(Stream.empty, 0))
        }
      }
      // retry
      tok <- inner
    } yield tok
  }

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

  lazy val eol: Parser[CharacterToken] =
    for {
      (ch @ CharacterToken(_, Category.END_OF_LINE)) <- any
    } yield ch

  lazy val whitespace: Parser[CharacterToken] =
    for {
      (ch @ CharacterToken(value, cat)) <- next
      if cat == Category.SPACE || cat == Category.END_OF_LINE
    } yield ch

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
      (cs @ ControlSequenceToken(_, _)) <- next
    } yield cs

  /** Parser that accepts any control sequence, without performing any expansion */
  lazy val rawControlSequence: Parser[ControlSequenceToken] =
    for {
      (cs @ ControlSequenceToken(_, _)) <- any
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
        case ControlSequenceToken(name, _) :: rest =>
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
