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

import org.parboiled._
import org.parboiled.scala.{ Rule1, Input }
import org.parboiled.parserunners.AbstractParseRunner
import org.parboiled.buffers._
import org.parboiled.support._

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

  def foreach(f: Token => Unit) = {

    val runner = new InternalParseRunner

  }

  def run: MonadicParsingResult[Token] = {
    EmptyResult
  }

  private class InternalParseRunner extends AbstractParseRunner[Token](rule)
      with MatchHandler {

    // the stack containing the expanded tokens if any
    // as long as this stack is not empty, the runner does not read more tokens
    // from the input but consumes these ones
    private val expanded = Stack.empty[Token]

    // the root context
    private var rootContext: MatcherContext[Token] = null

    // 
    private var inputBuffer: InputBuffer = null

    def run(input: InputBuffer) = {

      // clear the stack
      expanded.clear
      // set the new context
      rootContext = createRootContext(input, this, true)
      // save the input
      inputBuffer = input

      // run this runner once
      run

    }

    def run = {
      // empty the value stack
      resetValueStack

      val matched = if (expanded.isEmpty) {
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

    def `match`(context: MatcherContext[_]) = {
      context.getMatcher.`match`(context)
    }

    private def expand(token: Token) = token // TODO

  }
}