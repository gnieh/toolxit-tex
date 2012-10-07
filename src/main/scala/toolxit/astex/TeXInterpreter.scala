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

import java.io.InputStream

import parser._

/** The TeX interpreter (or ''stomach'') consumes expanded tokens
 *  available  from an input stream and interprets them.
 *
 *  @author Lucas Satabin
 *
 */
class TeXInterpreter(is: InputStream) {

  // ==== internals ====

  // new empty environment
  val environment = new TeXEnvironment

  // build the token stream
  private val parser = new TeXParserImpl(environment)

  // the mode the TeX interpreter is currently in
  private var mode = Mode.VerticalMode

  import environment._

  def interpretAll = {
    while (interpret) { println("**") }
  }

  def interpret = {
    true
  }

}