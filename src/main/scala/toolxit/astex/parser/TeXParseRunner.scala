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

import scala.collection.mutable.Stack
import org.parboiled.buffers.InputBuffer
import org.parboiled.scala._
import org.parboiled.support.Chars

/** A parse runner that performs macro expansion where needed. Thus, returned
 *  control sequence tokens are either primitive control sequences or unknown
 *  control sequences.
 *  This parse runner is given the input only in the constructor (together with the rule)
 *  and result is returned using the parameter-less `run` method. This allows the user
 *  to invoke the runner several times, each time consuming the appropriate number of
 *  input character and performing macro expansion.
 *
 *  @author Lucas Satabin
 *
 */
class TeXParseRunner(parser: TeXParser, input: InputBuffer) {

  /** Iterates over the generated results until the end of input is reached.
   *  Parsing results given to the function are non empty
   *  (i.e. not [[toolxit.astex.parser.EmptyResult]]).
   *  This method '''always''' iterates over the entire input, even if
   *  the `run` method was already called several times.
   */
  def foreach(f: NonEmptyMonadicParsingResult[Token] => Unit) {
    // create a new fresh runner for the input
    val runner = new TokenIterator
    // iterate over the tokens until the end of input is reached
    // and execute the given function on each returned token
    @scala.annotation.tailrec
    def iterate {
      runner.nextToken match {
        case EmptyResult => // EOI reached, stop iteration
        case NonEmptyMonadicParsingResult(result) =>
          // some result was returned
          // apply function to it
          f(result)
          // iterate
          iterate
      }
    }
    iterate
  }

  /** Parses and returns one more (expanded) token, returns
   *  [[tookxit.astex.parser.EmptyResult]] if the end of input was reached.
   *  This method may be invoked several times, the input will be parsed repeatedly
   *  until its end.
   */
  def run: MonadicParsingResult[Token] =
    iterator.nextToken

  // ================ internals ================
  private lazy val iterator = new TokenIterator

  private class TokenIterator {

    private var inputBuffer = input

    // the stack containing the expanded tokens if any
    // as long as this stack is not empty, the runner does not read more tokens
    // from the input but consumes these ones
    private val expanded =
      Stack.empty[Token]

    def nextToken =
      if (expanded.isEmpty) {
        // run the matcher, no tokens already expanded in the stack
        parseWith(parser.token)
      } else {
        // push the token onto the stack that is the result of a previous expansion
        OkResult(expanded.pop)
      }.flatMap(expand)

    import parser.environment._

    private var noexpand = false

    private def expand(token: Token): MonadicParsingResult[Token] = token match {
      case ControlSequenceToken(name) if shallExpand(name) =>
        css(name) match {
          case Some(TeXMacro(_, parameters, replacement)) =>
            // parse arguments
            parseWith(parser.argumentParser(parameters)).map { arguments =>
              // replace with the replacement text
              val replaced = replacement.flatMap {
                case ParameterToken(index) =>
                  arguments(index - 1)
                case token => List(token)
              }
              expanded.pushAll(replaced.reverse)
              expanded.pop
            }
          case Some(TeXInteger(_, value)) =>
            OkResult(token)
          case Some(TeXBuiltin("noexpand")) =>
            // inhibit macro expansion
            noexpand = true
            // get the next token
            nextToken.map { token =>
              // re-establish macro expansion
              noexpand = false
              NotExpandedToken(token)
            }
          case Some(TeXBuiltin(name)) =>
            OkResult(token)
          case _ =>
            OkResult(token)
        }
      case _ => OkResult(token)
    }

    /* determines whether the control sequence shall be expanded
     * the result might depend on the current context
     */
    private def shallExpand(name: String) = {
      if (noexpand) {
        false
      } else {
        // retrieve the control sequence
        css(name) match {
          case Some(TeXMacro(_, _, _)) =>
            true
          case Some(cs) if Primitives.expandablePrimitives.contains(cs.name) =>
            true
          case _ => false
        }
      }
    }

    private def parseWith[T](rule: Rule1[T]) = {
      if (inputBuffer.charAt(0) == Chars.EOI) {
        // end of input reached
        EmptyResult
      } else {
        // something to parse
        val r = ReportingParseRunner(parser.withRemaining(rule)).inner
        MonadicParsingResult(r.run(inputBuffer)).map {
          case (token, input) =>
            inputBuffer = input
            token
        }
      }
    }
  }
}
