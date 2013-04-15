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

/** A bunch of parsers that transform and expand the TeX tokens. The results
 *  are primitive TeX commands. The parsers must perform macro expansion when needed.
 *
 *  @author Lucas Satabin
 */
abstract class TeXParsers extends Parsers[Token] {

  type State = TeXState

  case class TeXState(stream: Stream[Token], pos: Pos, env: TeXEnvironment) extends Input

  protected def makeState(old: State, stream: Stream[Token], pos: Pos): State =
    old.copy(stream = stream, pos = pos)

  /** Parser that parses a command, performing all needed expansions */
  lazy val command: Parser[Command] =
    // TODO implement
    fail("not implemented yet")

}
