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

import eyes._
import mouth._

/** A parser that reads lazy stream of TeX input.
 *
 *  @author Lucas Satabin
 */
case class StreamTeXParser(input: Stream[Char], inputResolver: String => Option[Stream[Char]]) extends TeXParser {
  self =>

  val env = new TeXEnvironment(None)

  /** The stream of primitive TeX commands.
   *  Macro expansion was performed as needed so that you have reduced commands */
  lazy val commands: Stream[Command] =
    mouth.stream(mouth.command, eyes.stream(eyes.token, input))

  // ========== internals ==========

  protected[this] val eyes = new TeXEyes with StreamProcessor[Char] {

    protected def createState(input: Stream[Char]): State =
      TeXEyesState(input, CharPosition(None, 0, 1, 1), ReadingState.N, env)

  }

  protected[this] val mouth = new TeXMouth with StreamProcessor[Token] {

    protected def createState(input: Stream[Token]): State =
      TeXState(input, TokenPosition(None, 0, 1, 1), env)

    protected def resolve(name: String): Option[Stream[Token]] =
      inputResolver(name) map { chars =>
        eyes.stream(eyes.token, chars)
      }

  }

}
