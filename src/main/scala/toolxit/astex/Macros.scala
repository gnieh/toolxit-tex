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

sealed trait Macro

/** A user defined macro has a name (control sequence) and a list of
 *  parameters. Whenever this sequence occurs, it is replace by the
 *  replacement tokens.
 *
 *  @author Lucas Satabin
 *
 */
final case class UserMacro(cs: ControlSequenceToken,
                           parameters: List[Token],
                           replacement: List[Token],
                           long: Boolean = false,
                           outer: Boolean = false)
    extends Macro

/** A primitive macro is already installed into the TeX program at the beginning. */
final case class PrimitiveMacro(cs: ControlSequenceToken,
                                parameters: List[Token],
                                run: List[Token] => Option[Token])
    extends Macro