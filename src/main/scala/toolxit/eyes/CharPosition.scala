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
package eyes

import util.StreamPosition

/** A position that has no context in the stream for what was read before and
 *  what comes next. It is only aware of the current token
 *
 *  @author Lucas Satabin
 */
case class CharPosition(read: Option[Char], offset: Int, line: Int, column: Int) extends StreamPosition[Char] {

  def next(read: Char): CharPosition =
    if(read == '\n')
      CharPosition(Some(read), offset + 1, line + 1, 1)
    else
      CharPosition(Some(read), offset + 1, line, column + 1)

}
