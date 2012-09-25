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

import org.parboiled.{ MatchHandler, MatcherContext }
import org.parboiled.scala.{ Rule1, Input }
import org.parboiled.parserunners.AbstractParseRunner
import org.parboiled.buffers.InputBuffer
import org.parboiled.support.ParsingResult
import org.parboiled.errors.ParseError

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
class TeXParseRunner(rule: Rule1[Token], input: Input) {

  private val runner = new InternalParseRunner

  /** Iterates over the generated results until the end of input is reached.
   *  Parsing results given to the function are non empty
   *  (i.e. not [[toolxit.astex.parser.EmptyResult]]).
   *  This method '''always''' iterates over the entire input, even if
   *  the `run` method was already called several times.
   */
  def foreach(f: NonEmptyMonadicParsingResult[Token] => Unit) {

    val runner = new InternalParseRunner

    // iterate over the tokens until the end of input is reached
    // and execute the given function on each returned token
    @scala.annotation.tailrec
    def iterate {
      MonadicParsingResult(runner.run) match {
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
    MonadicParsingResult(runner.run)

  private class InternalParseRunner extends AbstractParseRunner[Token](rule)
      with MatchHandler {

    // the stack containing the expanded tokens if any
    // as long as this stack is not empty, the runner does not read more tokens
    // from the input but consumes these ones
    private val expanded =
      Stack.empty[Token]

    // the input buffer
    private var inputBuffer: InputBuffer =
      input.inputBuffer

    // the root context
    private var rootContext: MatcherContext[Token] =
      createRootContext(inputBuffer, this, true)

    // never invoked, only there to conform to the interface
    def run(input: InputBuffer) =
      throw new UnsupportedOperationException("InternalParseRunner.run(InputBuffer) shall never be called")

    def run = {
      // empty the value stack
      resetValueStack

      if (rootContext.getCurrentIndex >= input.input.size) {
        null
      } else {
        val matched = if (expanded.isEmpty) {
          // run the matcher, no tokens already expanded in the stack
          rootContext.runMatcher
        } else {
          // push the already expanded token onto the stack
          getValueStack.push(expand(expanded.pop))
          // the runner matched a token
          true
        }
        // create and return the result
        new ParsingResult(matched, null, getValueStack, getParseErrors, inputBuffer)
      }
    }

    def `match`(context: MatcherContext[_]) = {
      context.getMatcher.`match`(context)
    }

    private def expand(token: Token) = token // TODO

  }
}