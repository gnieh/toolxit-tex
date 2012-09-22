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

import java.io.{ InputStream, PushbackInputStream }

import scala.collection.mutable.Map

/** Lazily builds a token stream from an character stream.
 *  A token stream returns the token read in the user input.
 *  It performs macro expansion when some are encountered.
 *  In the beginning, it is initialized and only the following category code exists:
 *   - `<return>` has category 5 (end of line)
 *   - `<space>` has category 10 (space)
 *   - `<null>` has category 9 (ignored character)
 *   - `<delete>` has category 15 (invalid character)
 *   - the UTF-8 letters have category 11 (letter)
 *   - `%` has category 14 (comment character)
 *   - `\`has category 0 (escaped character)
 *  This means that initially there is no grouping capability.
 *
 *  Tokens produced by this string are expanded. This means, that when the returned token is
 *  a control sequence, clients may be sure that this is a primitive macro that can be
 *  executed as such. According to the TeX book, the parameter for user defined macros
 *  are expanded in a `call-by-name` way, i.e. if a user defined macro is one of the parameters
 *  of another user defined macro, the parameter is not evaluated before it is used in the
 *  expanded macro text. If the parameter is not used, then it is never evaluated.
 *  Expansion is performed as stated in the chapter 20 of ''The TeX Book''. Shortly these
 *  are the rules summarizing when a control sequence is not expanded:
 *   - during error recovery
 *   - when tokens are skipped in an ignored conditional branch
 *   - when reading macro arguments and arguments of some primitives
 *   - when reading parameter list of a macro being defined
 *   - when reading the replacement text for defined macros in some cases
 *   - right after reading a `$` to determine whether the next token is also a `$`
 *   - right after reading a ``` for defining an alphabetic constant
 *
 *  @author Lucas Satabin
 *
 */
class TeXTokenStreamBuilder(is: InputStream,
                            environment: TeXEnvironment,
                            reportMessage: (Level.Value, Int, Int, String) => Unit) {

  /** Transforms this parser to a token stream. */
  lazy val toStream =
    Stream.continually(nextToken).takeWhile(_.isDefined).map(_.get)

  // == internals ==

  // the internal raw input characters
  private var internalStream =
    Stream.continually(is.read).takeWhile(_ != -1).map(_.toChar)

  // the input stream converted into a stream of UTF-8 characters
  private var inputStream: Stream[Char] =
    Stream.continually(nextChar).takeWhile(_.isDefined).map(_.get)

  // the state in which the reader is
  private var state = ReadingState.N

  // when this flag is set, the next token will not be expanded
  private var noexpand = false

  // when this flag is set, it indicates that the parser is deleting
  // tokens in during error recovery
  private[astex] var recovery = false

  // when this flag is set, it indicates, that the parser is currently
  // reading a definition (e.g. a macro definition)
  private[astex] var definition = false

  // when this flag is set, and `definition` is also set, the parser
  // will accept long macro definition
  private[astex] var longdef = false

  // when this flag is set, it indicates, that the parser is currently
  // reading an argument list
  private[astex] var arguments = false

  import environment._

  // line and column information for reporting
  private var line = 1
  private var column = 1

  /* returns the next token from the input stream */
  @scala.annotation.tailrec
  private def nextToken: Option[Token] = inputStream.headOption match {
    case Some(read) =>
      val cat = category(read)
      // the read character is consumed
      consume(1)
      if (cat == Category.ESCAPE_CHARACTER) {
        // if the read character is an escape character
        // (whatever the state is), we want to scan a
        // control sequence name
        val cs = ControlSequenceToken(controlSequenceName)
        // now that we defined the control sequence name,
        // we might have to expand it
        if (shallExpand(cs)) {
          expand(cs)
          nextToken
        } else {
          Some(cs)
        }
      } else if (cat == Category.END_OF_LINE) {
        if (state == ReadingState.N) {
          // this is a new paragraph
          line += 1
          column = 1
          Some(ControlSequenceToken("par"))
        } else if (state == ReadingState.M) {
          // this is a space
          state = ReadingState.N
          line += 1
          column = 1
          Some(CharacterToken(' ', Category.SPACE))
        } else {
          // state S, just drop this character and read the next one
          nextToken
        }
      } else if (cat == Category.IGNORED_CHARACTER) {
        // ignore this character and read the next one
        // as if this one was not there
        nextToken
      } else if (cat == Category.SPACE) {
        if (state == ReadingState.M) {
          // switch to reading state S and return the space character
          state = ReadingState.S
          Some(CharacterToken(' ', Category.SPACE))
        } else {
          // simply ignore this space token and remain in the same state
          // go to the next token
          nextToken
        }
      } else if (cat == Category.COMMENT_CHARACTER) {
        // consume and ignore all characters until the end of the line
        consume(inputStream.takeWhile(c => category(c) != Category.END_OF_LINE).size + 1)
        nextToken
      } else if (cat == Category.INVALID_CHARACTER) {
        // report an error 
        reportMessage(Level.ERROR, line, column, "invalid character found: " + read)
        // and continue
        nextToken
      } else {
        // normal case
        // middle of the line
        state = ReadingState.M
        // simple character token is returned with attached category
        Some(CharacterToken(read, cat))
      }
    case None =>
      // end of stream
      None
  }

  private def controlSequenceName: String = {
    inputStream.headOption match {
      case Some(read) =>
        if (category(read) == Category.LETTER) {
          // a letter
          val seq = inputStream.takeWhile(c => category(c) == Category.LETTER)
          // build the control sequence name
          val name = seq.mkString
          // consume the name
          consume(name.size)
          // switch to reading state S
          state = ReadingState.S
          //return the built name
          name
        } else {
          // not a letter, the name consists of that unique symbol
          // consume this symbol
          consume(1)
          inputStream.headOption match {
            case Some(c) if category(c) == Category.SPACE =>
              // switch to reading state S if next character is a space
              state = ReadingState.S
            case _ =>
              // switch to reading state M
              state = ReadingState.M
          }
          // return the symbol
          read.toString
        }
      case None =>
        // switch to reading state M
        state = ReadingState.M
        // end of line, empty control sequence name
        ""
    }

  }

  /* expand the given control sequence. this method may consume following tokens */
  private def expand(cs: ControlSequenceToken) {
    val name = cs.name
    if (Primitives.isIf(name)) {
      // TODO expand an if
    } else if (name == "") {
      // TODO all other primitive cases
    } else {
      // this must be probably a user defined control sequence
      css(name) match {
        // this is a user macro, expand it
        case Some(macro: UserMacro) => expandMacro(macro)
        case _ => // XXX shall never happen!!!
          throw new TeXException("I'd like to expand control sequence \\" + name
            + ", but this case seems not to be implemented")
      }
    }
  }

  /* expand the user defined macro */
  private def expandMacro(macro: UserMacro) {
    // first read the arguments

    @scala.annotation.tailrec
    def streamMatches(stream: Stream[Token], delimiter: List[Token]): Boolean = {
      delimiter match {
        case token :: rest =>
          stream.headOption match {
            case Some(tok) if tok == token =>
              streamMatches(stream.tail, rest)
            case _ => false
          }
        case Nil =>
          // empty delimiter, always match
          true
      }
    }

    def readArguments(parameters: List[List[Token]],
                      akk: List[List[Token]]): List[List[Token]] = {
      parameters match {
        case param :: rest =>
          param match {
            case List(ParameterToken(id)) =>
              // two cases
              rest match {
                case List(ParameterToken(_)) :: _ | Nil =>
                  // non delimited parameter
                  akk ::: List(List(nextToken.get))
                case delimiter :: _ =>
                  // delimited parameter, read until the next delimiter is found
                  do {
                    nextToken match {
                      case Some(brace @ CharacterToken(_, Category.BEGINNING_OF_GROUP)) =>
                        brace :: group
                      case Some(token) =>
                      case None =>
                    }
                  } while (true)
                  akk ::: List(List(nextToken.get))
              }
            case delimiter if streamMatches(toStream, delimiter) =>
              // consume the delimiter tokens
              consume(delimiter.size)
              // argument list stays unchanged
              readArguments(rest, akk)
            case _ =>
              throw new TeXException(nextToken + " does not match definition of control sequence " + macro.cs)
          }
        case Nil =>
          akk
      }
    }

    val arguments = macro.parameters.foldLeft(List[List[Token]]()) { (result, token) =>
      token match {
        case List(ParameterToken(id)) =>
        // two cases
        case delimiter if streamMatches(toStream, delimiter) =>
          // consume the delimiter tokens
          consume(delimiter.size)
          // argument list stays unchanged
          result
        case _ =>
          throw new TeXException(nextToken + " does not match definition of control sequence " + macro.cs)
      }
      result
    }
  }

  /* parse a group and return its corresponding token list (with closing brace). 
   * the entire group is consumed even if it is not properly closed
   * (in this case, the stream is read until the end of stream is reached) */
  private def group: List[Token] = {
    @scala.annotation.tailrec
    def readGroup(result: List[Token], depth: Int): List[Token] = {
      nextToken match {
        case Some(tok @ CharacterToken(_, Category.BEGINNING_OF_GROUP)) =>
          // nested group
          readGroup(result ::: List(tok), depth + 1)
        case Some(tok @ CharacterToken(_, Category.END_OF_GROUP)) =>
          // end of group
          if (depth == 0) {
            // if it is the top-level group, return it
            result ::: List(tok)
          } else {
            // else decrement the depth and continue reading
            readGroup(result ::: List(tok), depth - 1)
          }
        case Some(token) =>
          // some other token
          readGroup(result ::: List(token), depth)
        case None =>
          result
      }
    }
    readGroup(Nil, 0)
  }

  /* consumes n characters from the input stream */
  private def consume(n: Int) {
    inputStream = inputStream.drop(n)
  }

  /* determines whether the control sequence shall be expanded
   * the result might depend on the current context
   */
  private def shallExpand(cs: ControlSequenceToken) = {
    if (noexpand || recovery || definition || arguments) {
      false
    } else {
      // retrieve the control sequence
      css(cs.name) match {
        case Some(_: UserMacro) =>
          true
        case Some(cs) if Primitives.expandablePrimitives.contains(cs.cs) =>
          true
        case _ => false
      }
    }
  }

  private def tokenStream =
    Stream.continually(nextToken).takeWhile(_.isDefined)

  private object ReadingState extends Enumeration {
    // reading state for input reading
    // N = new line
    // M = middle of a line
    // S = skipping blanks
    val N, M, S = Value
  }

  // ========== character stream building ==========

  private val hexaLower = "0123456789abcdef"

  /* consumes n characters from the original input stream */
  private def consumeChar(n: Int) {
    internalStream = internalStream.drop(n)
    column += n
  }

  /* pushes the given character at the beginning of the original input stream */
  private def pushChar(c: Char) {
    val old = internalStream
    internalStream = c #:: old
  }

  /* returns the next character */
  @scala.annotation.tailrec
  private def nextChar: Option[Char] = {
    internalStream.headOption match {
      case Some(c) if category(c) == Category.SUPERSCRIPT =>
        // we need a lookahead of 4 characters
        val lookahead = internalStream.take(4).toList

        if (lookahead.size >= 3 && lookahead(1) == c) {
          // we have `^^` followed by one or more characters
          // check if it is of the form `^^XX` where X is one of `0123456789abcdef`
          // (case sensitive)
          if (lookahead.size == 4
            && hexaLower.contains(lookahead(2))
            && hexaLower.contains(lookahead(3))) {
            // we are in the right case
            // consume the 4 characters
            consumeChar(4)
            val char = (lookahead(2) << 4 + lookahead(3)).toChar
            pushChar(char)
            // recursive call with new first character
            nextChar
          } else if (lookahead(2) < 128) {
            // it is of the form `^^A`
            val letter = lookahead(2)
            val code = if (letter < 64) {
              letter + 64
            } else {
              letter - 64
            }
            // consume the 3 characters
            consumeChar(3)
            // replace the head character
            pushChar(code.toChar)
            // recursive call with new first character
            nextChar
          } else {
            // other case, simply return the character
            consumeChar(1)
            Some(c)
          }
        } else {
          // other case, simply return the character
          consumeChar(1)
          Some(c)
        }
      case Some(c) =>
        // other case, simply return the character
        consumeChar(1)
        Some(c)
      case None =>
        None
    }
  }

}