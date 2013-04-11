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
case class StreamTeXParser(input: Stream[Char]) extends TeXParser {
  self =>

  val env = new TeXEnvironment(None)

  // TODO
  lazy val commands: Stream[Command] = {
    Stream.Empty
  }

  // ========== internals ==========

  private def characters(input: lexer.TeXLexerState): Stream[Token] = {
    import lexer._
    run(token, input) match {
      case Success(tok, rest, _) =>
        tok #:: characters(rest)
      case Error(msg) =>
        throw new TeXException("")
    }
  }

  protected[this] val lexer = new TeXLexers[StreamPosition[Char]] {
    protected def nextPos(current: StreamPosition[Char], read: Char): StreamPosition[Char] =
      current.next
  }

  protected[this] val parser = new TeXParsers[StreamPosition[Token]] {
    protected def nextPos(current: StreamPosition[Token], read: Token): StreamPosition[Token] =
      current.next
  }

}
