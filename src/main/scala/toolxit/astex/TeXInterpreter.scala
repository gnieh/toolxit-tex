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
  private val parser = new TeXTokenStreamBuilder(is, environment, (_, _, _, _) => ())
  private var stream = parser.toStream

  // the mode the TeX interpreter is currently in
  private var mode = Mode.VerticalMode

  import environment._

  def interpretAll = {
    while (interpret) { println("**") }
  }

  def interpret = {
    stream.headOption match {
      case Some(cs: ControlSequenceToken) =>
        // we encountered a control sequence
        // first fetch it from environment
        css(cs.name) match {
          case Some(PrimitiveMacro(_, code, _, _)) =>
            // execute the code of this primitive macro
            stream = code(stream)
          case _ =>
            // Should *NEVER* happen as the stream was correctly expanded
            throw new ControlSequenceException("WTF is this control sequence: " + cs.name + "???")
        }
        true
      case Some(CharacterToken(_, Category.BEGINNING_OF_GROUP)) =>
        consume(1)
        println("entering group")
        enterGroup
        true
      case Some(CharacterToken(_, Category.END_OF_GROUP)) =>
        consume(1)
        println("leaving group")
        leaveGroup
        true
      case Some(token) =>
        consume(1)
        println(token)
        true
      case None =>
        // end of stream
        false
    }
  }

  private def consume(n: Int) {
    stream = stream.drop(n)
  }

}