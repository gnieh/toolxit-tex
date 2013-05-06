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

import dimen._

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
class TeXEnvironment private[toolxit] (parent: Option[TeXEnvironment]) {
  self =>

  /** The root environment of this instance */
  val root: TeXEnvironment = parent match {
    case Some(env) => env.root
    case None      => this
  }

  val jobname: String =
    root.jobname

  /** Enters a new group and returns the new environment local to this group. */
  def enterGroup =
    new TeXEnvironment(Some(this))

  /** Leaves a group and returns the environment corresponding to the parent group. */
  def leaveGroup =
    parent match {
      case Some(env) => env
      case None => throw new TeXException("Root environment has no parent")
    }

  /** Exposes category management functions. */
  object category {
    /** Returns the category of the given character in the current environment.
     *  This category may vary over time, so this method must be called every time
     *  one needs to determine the category of a character.
     */
    def apply(char: Char): Category.Value =
      categories.get(char) match {
        case Some(cat) => cat
        case None =>
          parent match {
            case Some(p) => p.category(char)
            case None =>
              // if not specified otherwise, UTF-8 letters are in category `letter`
              if (char.isLetter)
                Category.LETTER
              else
                Category.OTHER_CHARACTER
          }
      }

    /** Sets the category of the given character. This setting is scoped
     *  to the current group only, and will be discarded when leaving the group.
     */
    def update(char: Char, category: Category.Value) =
      categories(char) = category
  }

  /** Exposes control sequence management functions. */
  object css {

    /** Exposes global control sequence management functions. */
    object global {
      /** Finds and returns the control sequence definition identified by its name.
       *  If the control sequence is not found, returns `None`.
       */
      def apply(name: String): Option[ControlSequence] =
        root.css(name)

      /** Adds or replace the global control sequence identified by the given name
       *  with the new control sequence definition. This control sequence definition
       *  is global and so will be available in any context.
       */
      def update(name: String, cs: ControlSequence) =
        root.css(name) = cs
    }

    /** Finds and returns the control sequence definition identified by its name.
     *  If the control sequence is not found in the given context, returns `None`.
     */
    def apply(name: String): Option[ControlSequence] = controlSequences.get(name) match {
      case Some(cs) => Some(cs)
      case None =>
        parent match {
          case Some(p) => p.css(name)
          case None => None
        }
    }

    /** Adds or replace the control sequence identified by the given name
     *  with the new control sequence definition. This control sequence definition
     *  is scoped to  the current group only, and will be discarded when leaving the group.
     */
    def update(name: String, cs: ControlSequence) =
      controlSequences(name) = cs
  }

  /** Exposes count register management functions. */
  object count {

    /** Finds and returns the count register value identified by its register number
     *  in the current environment.
     *  The default value of a count register is `0`.
     */
    def apply(number: Byte): Int = counters.get(number) match {
      case Some(n) => n
      case None =>
        parent match {
          case Some(p) => p.count(number)
          case None =>
            throw new TeXInternalException("unknown counter: " + number)
        }
    }

    /** Sets the value of the count register in the current environment.
     *  This value will be reseted to the previous value when leaving the current group.
     */
    def update(number: Byte, value: Int) =
      counters(number) = value
  }

  /** Exposes dimension register management functions. */
  object dimen {

    /** Finds and returns the dimension register value identified by its register number
     *  in the current environment.
     *  The default value of a dimension register is `0 pt`.
     */
    def apply(number: Byte): Dimension = dimensions.get(number) match {
      case Some(d) => Dimension(d)
      case None =>
        parent match {
          case Some(p) => p.dimen(number)
          case None =>
            throw new TeXInternalException("unknown dimension: " + number)
        }
    }

    /** Sets the value of the dimension register in the current environment.
     *  This value will be reseted to the previous value when leaving the current group.
     */
    def update(number: Byte, value: Dimension) =
      dimensions(number) = value.sps
  }

  /** Exposes glue register management functions. */
  object skip {

    /** Finds and returns the glue register value identified by its register number
     *  in the current environment.
     *  The default value of a glue register is `0 pt +0 pt -0 pt`.
     */
    def apply(number: Byte): Glue = glues.get(number) match {
      case Some(g) => Glue(g._1, g._2, g._3)
      case None =>
        parent match {
          case Some(p) => p.skip(number)
          case None =>
            throw new TeXInternalException("unknown skip: " + number)
        }
    }

    /** Sets the value of the count register in the current environment.
     *  This value will be reseted to the previous value when leaving the current group.
     */
    def update(number: Byte, value: Glue) =
      glues(number) = (value.value, value.stretch, value.shrink)
  }

  /** Exposes muglue register management functions. */
  object muskip {

    /** Finds and returns the muglue register value identified by its register number
     *  in the current environment.
     *  The default value of a muglue register is `0 pt +0 pt -0 pt`.
     */
    def apply(number: Byte): Muglue = muglues.get(number) match {
      case Some(g) => Muglue(g._1, g._2, g._3)
      case None =>
        parent match {
          case Some(p) => p.muskip(number)
          case None =>
            throw new TeXInternalException("unknown muskip: " + number)
        }
    }

    /** Sets the value of the count register in the current environment.
     *  This value will be reseted to the previous value when leaving the current group.
     */
    def update(number: Byte, value: Muglue) =
      muglues(number) = (value.value, value.stretch, value.shrink)
  }

  /** Gets the current escape character */
  def escapechar: CharacterToken =
    _escapechar match {
      case Some(c) =>
        c
      case None =>
        parent match {
          case Some(p) =>
            p.escapechar
          case None =>
            throw new TeXException("should never happen")
        }
    }

  def escapechar_=(c : Char) {
    _escapechar = Some(CharacterToken(c, Category.OTHER_CHARACTER))
  }

  /** Returns the meaning of the given token in the current environment */
  def meaning(token: Token): String = token match {
    case CharacterToken(c, Category.BEGINNING_OF_GROUP) =>
      "begin-group character " + c
    case CharacterToken(c, Category.END_OF_GROUP) =>
      "end-group character" + c
    case CharacterToken(c, Category.OTHER_CHARACTER) =>
      "the character " + c
    case CharacterToken(c, Category.LETTER) =>
      "the letter " + c
    case CharacterToken(c, Category.PARAMETER) =>
      "macro parameter character " + c
    case ControlSequenceToken(name, _) if Primitives.all.contains(name) =>
      escapechar.value + name
    case ControlSequenceToken(name, _) =>
      val esc = escapechar.value
      css(name) match {
        case Some(cs) =>
          cs match {
            case TeXInteger(_, number) =>
              esc + "count" + number
            case TeXChar(_, number) =>
              esc + "char\"" + number
            case TeXMathChar(_, number) =>
              esc + "mathchar\"" + number
            case TeXDimension(_, number) =>
              esc + "dimen" + number
            case TeXGlue(_, number) =>
              esc + "skip" + number
            case TeXMuglue(_, number) =>
              esc + "muskip" + number
            case TeXMacro(name, parameters, repl, long) =>
              val params = parameters map {
                case Left(ParameterToken(n)) =>
                  "#" + n
                case Right(tokens) =>
                  tokens.map(toString).mkString
              }
              "macro:" + params + "->" + repl.map(toString).mkString
            case TeXTokenList(_, number) =>
              esc + "toks" + number
            case TeXFont(_, number) =>
              ???
            case TeXPrimitive(name) =>
              esc + name
          }
        case None =>
          // unknown control sequence in this environment, undefined
          "undefined"
      }
  }

  /** Makes a string out of a parsed token */
  def toString(token: Token): String = token match {
    case ControlSequenceToken(name, true) =>
      // an active character is printed without escape character
      name
    case ControlSequenceToken(name, false) =>
      escapechar + name
    case CharacterToken(c, _) =>
      c.toString
    case _ =>
      throw new TeXInternalException("should never happen")
  }

  def toString(tokens: List[Token]): String =
    tokens.foldLeft("") { (acc, token) =>
      acc + toString(token)
    }

  // ==== internals ====

  // the internal parameters
  private[this] var _escapechar: Option[CharacterToken] = None


  // the map from character to category code
  private val categories = Map.empty[Char, Category.Value]

  // the map from cs name to its internal representation
  private val controlSequences = Map.empty[String, ControlSequence]

  // local values of the different registers
  private[this] val counters = Map.empty[Byte, Int]
  // all dimension are stored as an integer multiple of one sp
  // the biggest dimension accepted by TeX is 2^30sp, so an integer
  // is sufficient to store it.
  private[this] val dimensions = Map.empty[Byte, Int]
  // glues and muglues are the triple (dimension, stretch, shrink)
  private[this] val glues = Map.empty[Byte, (Int, Int, Int)]
  private[this] val muglues = Map.empty[Byte, (Int, Int, Int)]
  // TODO other register types

}

class RootTeXEnvironment(override val jobname: String) extends TeXEnvironment(None) {
  // set specific categories statically known at the beginning
  // when a fresh root environment is created
  category('\n') = Category.END_OF_LINE
  category(' ') = Category.SPACE
  category(0) = Category.INVALID_CHARACTER
  category('%') = Category.COMMENT_CHARACTER
  category('\\') = Category.ESCAPE_CHARACTER

  // default escape character
  escapechar = 92.toChar

  // add all primitive control sequence names
  for(name <- Primitives.all)
    css(name) = TeXPrimitive(name)

}
