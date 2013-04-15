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

/** A bunch of parsers that define TeX element such as
 *   - user macros
 *   - counters
 *   - fonts
 *   - dimensions
 *   - ...
 *
 *  @author Lucas Satabin
 */
trait TeXDefinitionParsers extends Parsers[Token] {
  this: TeXParsers =>

  lazy val texdef: Parser[TeXMacro] =
    for {
      _ <- controlSequence("def")
      ControlSequenceToken(name) <- controlSequence
      params <- parameterText
      _ <- beginningOfGroup
      replacement <- replacementText
      _ <- endOfGroup
    } yield TeXMacro(name, params, replacement)

  lazy val parameterText: Parser[List[Parameter]] =
    until(parameter, beginningOfGroup)

  lazy val parameter: Parser[Parameter] =
    fail("not implemented yet")

  lazy val replacementText: Parser[List[Token]] =
    fail("not implemented yet")

}
