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

import scala.collection.mutable.Map

/** The TeX environment tracks the following elements:
 *   - defined macros,
 *   - character category codes,
 *   - counters
 *
 *  Environments have a lexical scope, this means that a new sub environment
 *  is created whenever one encounters a character token of category `BEGINNING_OF_GROUP`
 *  and this sub environment is discarded on the first character token of category
 *  `END_OF_GROUP`.
 *  This hierarchy mechanism is fully managed by this class by calling the methods
 *  `enterGroup` and `leaveGroup`.
 *
 *  @see [[toolxit.astex.Category]]
 *
 *  @author Lucas Satabin
 *
 */
class TeXEnvironment {
  self =>

  /** Enters a new group. */
  def enterGroup {
    environment = new Environment(Some(environment))
  }

  /** Leaves a group. */
  def leaveGroup {
    environment.parent match {
      case Some(env) => environment = env
      case None => throw new TeXException("Root environment has no parent")
    }
  }

  /** Exposes category management functions. */
  object category {
    /** Returns the category of the given character in the current environment.
     *  This category may vary over time, so this method must be called every time
     *  one needs to determine the category of a character.
     */
    def apply(char: Char) =
      environment.category(char)

    /** Sets the category of the given character. This setting is scoped
     *  to the current group only, and will be discarded when leaving the group.
     */
    def update(char: Char, category: Category.Value) =
      environment.setCategory(char, category)
  }

  /** Exposes control sequence management functions. */
  object css {
    /** Finds and returns the control sequence definition identified by its name.
     *  If the control sequence is not found in the given context, returns `None`.
     */
    def apply(name: String) =
      environment.findControlSequence(name)

    /** Adds or replace the control sequence identified by the given name
     *  with the new control sequence definition. This control sequence definition
     *  is scoped to  the current group only, and will be discarded when leaving the group.
     */
    def update(name: String, cs: ControlSequenceDef) =
      environment.addControlSequence(name, cs)
  }

  /** Exposes global control sequence management functions. */
  object global {
    /** Finds and returns the control sequence definition identified by its name.
     *  If the control sequence is not found, returns `None`.
     */
    def apply(name: String) =
      root.findControlSequence(name)

    /** Adds or replace the global control sequence identified by the given name
     *  with the new control sequence definition. This control sequence definition
     *  is global and so will be available in any context.
     */
    def update(name: String, cs: ControlSequenceDef) =
      root.addControlSequence(name, cs)
  }

  // ==== internals ====

  private[this] val root = new Environment
  private[this] var environment = root

  // set specific categories statically known at the beginning
  // when a fresh root environment is created
  category('\n') = Category.END_OF_LINE
  category(' ') = Category.SPACE
  category(0) = Category.INVALID_CHARACTER
  category('%') = Category.COMMENT_CHARACTER
  category('\\') = Category.ESCAPE_CHARACTER

  private class Environment(val parent: Option[Environment] = None) {
    // the map from character to category code
    private val categories = Map.empty[Char, Category.Value]
    // the map from cs name to its internal representation
    private val css = Map.empty[String, ControlSequenceDef]

    def category(c: Char): Category.Value = {
      categories.get(c) match {
        case Some(cat) => cat
        case None =>
          parent match {
            case Some(p) => p.category(c)
            case None =>
              // if not specified otherwise, UTF-8 letters are in category `letter`
              if (Character.isLetter(c))
                Category.LETTER
              else
                Category.OTHER_CHARACTER
          }
      }
    }

    def setCategory(c: Char, cat: Category.Value) {
      categories(c) = cat
    }

    def addControlSequence(name: String, cs: ControlSequenceDef) = {
      css(name) = cs
    }

    def findControlSequence(name: String): Option[ControlSequenceDef] = css.get(name) match {
      case Some(cs) => Some(cs)
      case None =>
        parent match {
          case Some(p) => p.findControlSequence(name)
          case None => None
        }
    }

  }

}