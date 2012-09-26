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

import org.parboiled.Context
import org.parboiled.scala._
import org.parboiled.buffers.{ InputBuffer, DefaultInputBuffer }

/** Builds a token stream from an input character stream.
 *  A token stream returns the token read in the user input.
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
class TeXParserImpl(val environment: TeXEnvironment)
    extends TokenParsers
    with MacroParsers {

  def withRemaining[T](r: Rule1[T]): Rule1[(T, InputBuffer)] = rule {
    r ~~> withContext { (value, context) =>
      val newStart = context.getMatchEndIndex
      val buffer = context.getInputBuffer match {
        case lazyBuffer: StreamInputBuffer =>
          // in this case just drop the read tokens
          lazyBuffer.drop(newStart)
        case buffer =>
          // build a new buffer minus the read characters
          new DefaultInputBuffer(buffer.extract(newStart, Int.MaxValue).toCharArray)

      }
      (value, buffer)
    }
  }

}