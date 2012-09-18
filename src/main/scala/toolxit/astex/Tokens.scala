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

/** TeX works with a token stream */
sealed trait Token

/** A character token read as input. It may be one of the folowing tokens:
 *   - escape character (by default `\`)
 *   - beginning of group (by default `{`)
 *   - end of group (by default `}`)
 *   - math shift (by default `$`)
 *   - alignment tab (by default `&`)
 *   - end of line (by default `\n`)
 *   - parameter (by default `#`)
 *   - superscript (by default `^`)
 *   - subscript (by default `_`)
 *   - ignored character (for example `null`)
 *   - space (such as ` `)
 *   - a letter (by default a UTF-8 encoded character)
 *   - active character (by default `~`)
 *   - comment character (by default `%`)
 *   - invalid character (<delete>)
 *   - other character (none of the above)
 *
 *  @author Lucas Satabin
 *
 */
case class CharacterToken(value: String, category: Category.Value) extends Token

/** A control sequence token has not category. */
case class ControlSequenceToken(name: String) extends Token