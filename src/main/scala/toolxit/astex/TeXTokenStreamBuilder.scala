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

import java.io.InputStream

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
 *  @author Lucas Satabin
 *
 */
class TeXTokenStreamBuilder(is: InputStream,
                            environment: TeXEnvironment,
                            reportMessage: (Level.Value, Int, Int, String) => Unit) {

  /** Transforms this parser to a token stream. */
  def toStream =
    Stream.continually(nextToken).takeWhile(_.isDefined).map(_.get)

  // == internals ==

  // the input stream converted into a stream of UTF-8 characters
  private var inputStream: Stream[Char] = new CharacterStream(is)

  // the state in which the reader is
  private var state = ReadingState.N

  import environment._

  // line and column information for reporting
  private var line = 1
  private var column = 1

  /* returns the next token from the input stream */
  @scala.annotation.tailrec
  private def nextToken: Option[Token] = {
    inputStream.headOption match {
      case Some(read) =>
        val cat = category(read)
        // the read character is consumed
        consume(1)
        if (cat == Category.ESCAPE_CHARACTER) {
          // if the read character is an escape character
          // (whatever the state is), we want to scan a
          // control sequence name
          Some(ControlSequenceToken(controlSequenceName))
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

  /* consumes n characters from the input stream) */
  private def consume(n: Int) {
    inputStream = inputStream.drop(n)
  }

  private object ReadingState extends Enumeration {
    // reading state for input reading
    // N = new line
    // M = middle of a line
    // S = skipping blanks
    val N, M, S = Value
  }

  /* this class represents the character stream read as input.
   * Special superscript characters are replaced
   */
  final private class CharacterStream(private var internalStream: Stream[Char]) extends Stream[Char] {

    def this(is: InputStream) = {
      this(Stream.continually(is.read).takeWhile(_ != -1).map(_.toChar))
    }

    private def hexaLower = "0123456789abcdef"

    def tailDefined = true

    override def isEmpty = internalStream.isEmpty

    @scala.annotation.tailrec
    override def head = {
      // we need a lookahead of 4 characters
      val lookahead = internalStream.take(4).toList
      lookahead.headOption match {
        case Some(c) =>
          if (category(c) == Category.SUPERSCRIPT
            && lookahead.size >= 3
            && lookahead(1) == c) {
            // we have `^^` followed by one or more characters
            // check if it is of the form `^^XX` where X is one of `0123456789abcdef`
            // (case sensitive)
            if (lookahead.size == 4
              && hexaLower.contains(lookahead(2))
              && hexaLower.contains(lookahead(3))) {
              // we are in the right case
              // consume the 4 characters
              consume(4)
              val char = (lookahead(2) << 4 + lookahead(3)).toChar
              pushCharacter(char)
              // recursive call with new first character
              head
            } else if (lookahead(2) < 128) {
              // it is of the form `^^A`
              val letter = lookahead(2)
              val code = if (letter < 64) {
                letter + 64
              } else {
                letter - 64
              }
              // consume the 3 characters
              consume(3)
              // replace the head character
              pushCharacter(code.toChar)
              // recursive call with new first character
              head
            } else {
              // other case, simply return the character
              consume(1)
              c
            }
          } else {
            c
          }
        case None =>
          throw new NoSuchElementException("empty.head")
      }
    }

    override def tail = {
      if (isEmpty) {
        throw new NoSuchElementException("empty.head")
      } else {
        // call `head` to replace special characters if any
        head
        new CharacterStream(internalStream.tail)
      }
    }

    /* consumes n characters from the input stream) */
    private def consume(n: Int) {
      internalStream = internalStream.drop(n)
      column += n
    }

    /* pushes the given character at the beginning of the input stream */
    private def pushCharacter(c: Char) {
      val old = internalStream
      internalStream = c #:: old
    }

  }

}