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
class TeXEnvironment extends PrimitiveControlSequences {

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

  /** Exposes macro management functions. */
  object macro {
    /** Finds and returns the macro identified by its name.
     *  If the macro is not found in the given context, returns `None`.
     */
    def apply(name: String) =
      environment.findMacro(name)

    /** Adds or replace the macro identified by the given name
     *  with the new macro definition. This macro definition is scoped to
     *  the current group only, and will be discarded when leaving the group.
     */
    def update(name: String, macro: Macro) =
      environment.addMacro(name, macro)
  }

  // ==== internals ====

  private[this] var environment = new Environment

  // set specific categories statically known at the beginning
  // when a fresh root environment is created
  category('\n') = Category.END_OF_LINE
  category(' ') = Category.SPACE
  category(0) = Category.INVALID_CHARACTER
  category('%') = Category.COMMENT_CHARACTER
  category('\\') = Category.ESCAPE_CHARACTER

  // install primitive control sequences
  intallPrimitives

  private class Environment(val parent: Option[Environment] = None) {
    // the map from character to category code
    private val categories = Map.empty[Char, Category.Value]
    // the map from macro name to its internal representation
    private val macros = Map.empty[String, Macro]

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

    def addMacro(name: String, macro: Macro) {
      macros(name) = macro
    }

    def findMacro(name: String): Option[Macro] = macros.get(name) match {
      case Some(m) => Some(m)
      case None =>
        parent match {
          case Some(p) => p.findMacro(name)
          case None => None
        }
    }

  }

}