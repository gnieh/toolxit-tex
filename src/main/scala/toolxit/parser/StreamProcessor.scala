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

import scala.util.parsing.input.{
  Position,
  Positional
}

/** A stream processor is a special kind of parser that transforms an input stream into
 *  an output stream using on parser combinator. It positions the produced output, so
 *  they must implement the `scala.util.parsing.input.Positional` trait
 *
 *  @author Lucas Satabin
 */
trait StreamProcessor[Input, Pos <: Position, Output <: Positional] extends Parsers[Input, Pos] {

  val transformer: Parser[Output]

  def stream(input: State): Stream[Output] =
    if(input.stream.isEmpty)
      Stream.Empty
    else
      run(transformer, input) match {
        case Success(token, rest, Message(pos, _, _)) =>
          token.setPos(pos) #:: stream(rest)
        case Error(msg) =>
          throw new TeXException(msg.toString)
      }
}