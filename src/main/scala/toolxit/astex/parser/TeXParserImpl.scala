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

import org.parboiled.scala._

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
  with MacroParsers