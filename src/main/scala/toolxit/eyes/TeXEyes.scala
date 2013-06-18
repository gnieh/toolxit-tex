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
package eyes

import util._

/** A bunch of parsers that transform stream of characters into TeX tokens.
 *  This is a kind of lexer for TeX inputs (its eyes as defined in the TeX book)
 *
 *  @author Lucas Satabin
 */
abstract class TeXEyes extends Parsers[Char] {

  type State = TeXEyesState

  case class TeXEyesState(stream: Stream[Char], pos: Pos, readingState: ReadingState.Value, env: TeXEnvironment) extends Input

  protected def nextPos(current: Pos, read: Char): Pos =
    current.next(read)

  protected def makeState(old: State, stream: Stream[Char], pos: Pos): State =
    old.copy(stream = stream, pos = pos)

  /** any TeX token */
  lazy val token: Parser[Token] =
    for {
      // ignore skippable whitespaces ignored characters, and comments
      _ <- many(skipWhitespace <|> IGNORED_CHARACTER <|> comment)
      // then read the token and change reading state when needed
      tok <-
        // after control sequence, we go to reading state `skip whitespace`
        (for {
          t <- controlSequence <|> ACTIVE_CHARACTER
          () <- updateState(state => state.copy(readingState = ReadingState.S))
        } yield t) <|>
        EOL <|>
        (for {
          t <- character
          () <- updateState { state =>
            val st =
              if(t.category == Category.SPACE)
                // skip whitespaces after a space character
                ReadingState.S
              else
                // middle of the line after other character
                ReadingState.M
            state.copy(readingState = st)
          }
        } yield t)
    } yield tok

  /** A TeX comment token */
  lazy val comment: Parser[String] =
    for {
      _ <- COMMENT_CHARACTER
      state <- getState
      com <- many(anyTeX filter (c => state.env.category(c) != Category.END_OF_LINE))
      _ <- END_OF_LINE
    } yield com.mkString("")

  /** A TeX character token */
  lazy val character: Parser[CharacterToken] =
    ESCAPE_CHARACTER <|>
    BEGINNING_OF_GROUP <|>
    END_OF_GROUP <|>
    MATH_SHIFT <|>
    ALIGNMENT_TAB <|>
    END_OF_LINE <|>
    PARAMETER <|>
    SUPERSCRIPT <|>
    SUBSCRIPT <|>
    IGNORED_CHARACTER <|>
    SPACE <|>
    LETTER <|>
    OTHER_CHARACTER <|>
    COMMENT_CHARACTER <|>
    INVALID_CHARACTER

  /** A TeX control sequence token */
  lazy val controlSequence: Parser[ControlSequenceToken] =
    (for {
      _ <- ESCAPE_CHARACTER
      cs <- csname
    } yield ControlSequenceToken(cs)) <#> "control sequence"

  /** A character with category code 0 */
  lazy val ESCAPE_CHARACTER =
    (for {
      c <- anyTeX
      state <- getState
      cat = state.env.category(c)
      if cat == Category.ESCAPE_CHARACTER
    } yield CharacterToken(c, cat)) <#> "escape character (typically '\\')"

  /** A character with category code 1 */
  lazy val BEGINNING_OF_GROUP =
    (for {
      c <- anyTeX
      state <- getState
      cat = state.env.category(c)
      if cat == Category.BEGINNING_OF_GROUP
    } yield CharacterToken(c, cat)) <#> "beginning of group character (typically '{')"

  /** A character with category code 2 */
  lazy val END_OF_GROUP =
    (for {
      c <- anyTeX
      state <- getState
      cat = state.env.category(c)
      if cat == Category.END_OF_GROUP
    } yield CharacterToken(c, cat)) <#> "end of group character (typically '}')"

  /** A character with category code 3 */
  lazy val MATH_SHIFT =
    (for {
      c <- anyTeX
      state <- getState
      cat = state.env.category(c)
      if cat == Category.MATH_SHIFT
    } yield CharacterToken(c, cat)) <#> "math shift character (typically '$')"

  /** A character with category code 4 */
  lazy val ALIGNMENT_TAB =
    (for {
      c <- anyTeX
      state <- getState
      cat = state.env.category(c)
      if cat == Category.ALIGNMENT_TAB
    } yield CharacterToken(c, cat)) <#> "alignment tab character (typically '&')"

  /** A character with category code 5 */
  lazy val END_OF_LINE =
    (for {
      c <- anyTeX
      state <- getState
      cat = state.env.category(c)
      if cat == Category.END_OF_LINE
    } yield CharacterToken(c, cat)) <#> "end of line character"

  /** A character with category code 6 */
  lazy val PARAMETER =
    (for {
      c <- anyTeX
      state <- getState
      cat = state.env.category(c)
      if cat == Category.PARAMETER
    } yield CharacterToken(c, cat)) <#> "parameter character (typically '#')"

  /** A character with category code 7 */
  lazy val SUPERSCRIPT =
    (for {
      c <- anyTeX
      state <- getState
      cat = state.env.category(c)
      if cat == Category.SUPERSCRIPT
    } yield CharacterToken(c, cat)) <#> "superscript character (typically '^')"

  /** A character with category code 8 */
  lazy val SUBSCRIPT =
    (for {
      c <- anyTeX
      state <- getState
      cat = state.env.category(c)
      if cat == Category.SUBSCRIPT
    } yield CharacterToken(c, cat)) <#> "subscript character (typically '_')"

  /** A character with category code 9 */
  lazy val IGNORED_CHARACTER =
    (for {
      c <- anyTeX
      state <- getState
      cat = state.env.category(c)
      if cat == Category.IGNORED_CHARACTER
    } yield CharacterToken(c, cat)) <#> "ignored character"

  /** A character with category code 10 */
  lazy val SPACE =
    (for {
      c <- anyTeX
      state <- getState
      cat = state.env.category(c)
      if cat == Category.SPACE
    } yield CharacterToken(c, cat)) <#> "space character (typically ' ')"

  /** A character with category code 11 */
  lazy val LETTER =
    (for {
      c <- anyTeX
      state <- getState
      cat = state.env.category(c)
      if cat == Category.LETTER
    } yield CharacterToken(c, cat)) <#> "letter (typically, any UTF-8 letter will do)"

  /** A character with category code 12 */
  lazy val OTHER_CHARACTER =
    (for {
      c <- anyTeX
      state <- getState
      cat = state.env.category(c)
      if cat == Category.OTHER_CHARACTER
    } yield CharacterToken(c, cat)) <#> "other character (typically, characters like numbers, or parentheses)"

  /** A character with category code 13 */
  lazy val ACTIVE_CHARACTER =
    (for {
      c <- anyTeX
      state <- getState
      cat = state.env.category(c)
      if cat == Category.ACTIVE_CHARACTER
    } yield ControlSequenceToken(c.toString, true)) <#> "active character (typically '~')"

  /** A character with category code 14 */
  lazy val COMMENT_CHARACTER =
    (for {
      c <- anyTeX
      state <- getState
      cat = state.env.category(c)
      if cat == Category.COMMENT_CHARACTER
    } yield CharacterToken(c, cat)) <#> "comment character (typically '%')"

  /** A character with category code 15 */
  lazy val INVALID_CHARACTER =
    (for {
      c <- anyTeX
      state <- getState
      cat = state.env.category(c)
      if cat == Category.INVALID_CHARACTER
    } yield CharacterToken(c, cat)) <#> "invalid character"

  /** A space character. concatenates consecutive spaces into only one if the reading state is
   *  'new line' or 'skipping blank' */
  lazy val skipWhitespace =
    for {
      _ <- many1(SPACE)
      state <- getState
      if state.readingState == ReadingState.S || state.readingState == ReadingState.N
    } yield ()

  /* Parses a TeX character from a standard character. this parser transforms special characters sequences to single characters */
  lazy val anyTeX =
    ((for {
      // next character of the form `^^XX` where X is one of `0123456789abcdef`
      sup1 <- RAW_SUPERSCRIPT
      sup2 <- RAW_SUPERSCRIPT
      if sup1 == sup2
      h1 <- satisfy(c => hexaLower.contains(c))
      h2 <- satisfy(c => hexaLower.contains(c))
    } yield ((hexaLower.indexOf(h1) << 4) + hexaLower.indexOf(h2)).toChar) <#> "^^XX where X is one of '0123456789abcdef'") <|>
    ((for {
      // next character of the form `^^A`
      sup1 <- RAW_SUPERSCRIPT
      sup2 <- RAW_SUPERSCRIPT
      if sup1 == sup2
      letter <- satisfy(_ < 128)
    } yield if(letter < 64) (letter + 64).toChar else (letter - 64).toChar) <#> "^^X where X is a letter") <|>
    // otherwise, just any character will do
    (any <#> "any character")

  lazy val EOL =
    (for {
      _ <- END_OF_LINE
      state <- getState
      if state.readingState != ReadingState.S
    } yield
      if(state.readingState == ReadingState.N) {
        // in 'new line' mode, this is considered as a `\par` control sequence
        ControlSequenceToken("par")
      } else {
        // we are in normal mode, consider that this is a space
        CharacterToken(' ', Category.SPACE)
      }
    ) post { (token, state) =>
      // if we were in normal mode, switch to 'new line mode'
      // do not copy the state in other case to avoid to create to many objects
      if(token == CharacterToken(' ', Category.SPACE))
        state.copy(readingState = ReadingState.N)
      else
        state
    }

  // ========== internals ==========

  private val hexaLower = "0123456789abcdef"

  private lazy val RAW_SUPERSCRIPT =
    for {
      c <- any
      state <- getState
      if state.env.category(c) == Category.SUPERSCRIPT
    } yield c

  private lazy val csname =
    (for {
      letters <- many1(LETTER)
    } yield letters.map(_.value).mkString("")) <|>
    (for {
      c <- anyTeX
    } yield c.toString)

}
