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

import scala.util.parsing.input.Position

/** A complete TeX parser that processes some input and returns the stream of primitive
 *  commands to execute. It abstracts over the input channel type, so that it could be
 *  an in-memory string, a file, a network channel, user interactive inputs, ...
 *
 *  @author Lucas Satabin
 */
trait TeXParser {

  /** The (possibly lazy, possibly infinite) primitive command stream  parsed
   *  by this parser. If an error is encountered, the approprirate error command
   *  is returned so that error recovery can be performed by caller */
  def commands: Stream[Command]

}
