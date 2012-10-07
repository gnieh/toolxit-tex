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

// ================ primitive control sequences ================

/** A primitive macro is already installed into the TeX program at the beginning. */
final case class PrimitiveMacro(run: Stream[Token] => Stream[Token])(
  long: Boolean = false,
  outer: Boolean = false)

///** A token list register contains a list of token used in replacement of this name */
//final case class TokenListRegister(cs: String,
//                                   replacement: List[Token]) extends ControlSequenceDef {
//  val primitive = true
//}
//
//final case class PrimitiveCounter(cs: String, value: Int) extends ControlSequenceDef {
//  val primitive = true
//}

// ================ user defined control sequences ================

/** A user defined macro has a name (control sequence) and a list of
 *  parameters. Whenever this sequence occurs, it is replace by the
 *  replacement tokens.
 *
 *  @author Lucas Satabin
 *
 */
final case class UserMacro(parameters: List[Parameter],
                           replacement: List[Token])(
                             long: Boolean = false,
                             outer: Boolean = false)

///** A user defined counter (with \countdef) */
//final case class UserCounter(cs: String, value: Int) extends ControlSequenceDef {
//  val primitive = false
//}
//
///** A user defined dimension (with \dimendef) */
//final case class UserDimension(cs: String, value: Dimension) extends ControlSequenceDef {
//  val primitive = false
//}
//
///** A user defined character (with \chardef) */
//final case class UserCharacter(cs: String, value: Char) extends ControlSequenceDef {
//  val primitive = false
//}
//
///** A user defined math character (with \mathchardef) */
//final case class UserMathCharacter(cs: String, value: Char) extends ControlSequenceDef {
//  val primitive = false
//}
//
///** A user defined font (with \fontdef) */
//final case class UserFont(cs: String, externalName: String) extends ControlSequenceDef {
//  val primitive = false
//}
//
///** A user defined font (with \skipdef) */
//final case class UserSkip(cs: String, value: Dimension) extends ControlSequenceDef {
//  val primitive = false
//}
//
///** A user defined font (with \muskipdef) */
//final case class UserMuskip(cs: String, value: Dimension) extends ControlSequenceDef {
//  val primitive = false
//}