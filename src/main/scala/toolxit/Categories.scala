/*
* This file is part of the ToolXiT project.
*
* Licensed under the Apache License = Value val Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing = Value val software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND = Value val either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package toolxit

/** A category in which a character is.
 *
 *  @author Lucas Satabin
 *
 */
object Category extends Enumeration {

  val ESCAPE_CHARACTER = Value(0) // 0
  val BEGINNING_OF_GROUP = Value(1) // 1
  val END_OF_GROUP = Value(2) // 2
  val MATH_SHIFT = Value(3) // 3
  val ALIGNMENT_TAB = Value(4) // 4
  val END_OF_LINE = Value(5) // 5
  val PARAMETER = Value(6) // 6
  val SUPERSCRIPT = Value(7) // 7
  val SUBSCRIPT = Value(8) // 8
  val IGNORED_CHARACTER = Value(9) // 9
  val SPACE = Value(10) // 10
  val LETTER = Value(11) // 11
  val OTHER_CHARACTER = Value(12) // 12
  val ACTIVE_CHARACTER = Value(13) // 13
  val COMMENT_CHARACTER = Value(14) // 14
  val INVALID_CHARACTER = Value(15) // 15

}
