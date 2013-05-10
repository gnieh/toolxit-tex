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

import scala.util.parsing.input.Position

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
    parser.stream(parser.command, lexer.stream(lexer.token, input))

  // ========== internals ==========

  protected[this] val lexer = new TeXLexers with StreamProcessor[Char] {

    protected def createState(input: Stream[Char]): State =
      TeXLexerState(input, StreamPosition(input, 0), ReadingState.N, env)

  }

  protected[this] val parser = new TeXParsers with StreamProcessor[Token] {

    protected def createState(input: Stream[Token]): State =
      TeXState(input, StreamPosition(input, 0), env)

    protected def resolve(name: String): Option[Stream[Token]] =
      inputResolver(name) map { chars =>
        lexer.stream(lexer.token, chars)
      }

  }

}
