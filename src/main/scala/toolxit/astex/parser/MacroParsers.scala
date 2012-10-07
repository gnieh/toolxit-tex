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
package parser

import org.parboiled.scala._

/** Provides parsers for both:
 *   - parsing macro definitions
 *   - parsing macro invocations
 *
 *  @author Lucas Satabin
 *
 */
trait MacroParsers extends Parser {
  this: TokenParsers =>

  import environment._

  // =============== parsing macro invocations ===============

  /** the parameter text is a list of either parameter tokens or non brace characters */
  def parameterText: Rule1[List[Parameter]] = rule {
    // TODO add check to verify that parameters appear in correct order
    zeroOrMore(
      parameter ~~> (p => Left(p)) |
        zeroOrMore(!(parameter | BEGINNING_OF_GROUP | END_OF_GROUP) ~ character)
        ~~> (cs => Right(cs)))
  }

  /** parses a correctly balanced group */
  def group: Rule1[List[Token]] = rule {
    BEGINNING_OF_GROUP ~
      zeroOrMore(group | character ~~> (c => List(c))) ~ END_OF_GROUP ~~> { (open, block, close) =>
        CharacterToken(open, Category.BEGINNING_OF_GROUP) :: block.flatten ::: List(CharacterToken(close, Category.END_OF_GROUP))
      }
  }

  def argumentParser(parameters: List[Parameter]): Rule1[List[List[Token]]] = rule {

    def newRule(prefix: Rule1[List[List[Token]]], newr: Rule1[List[Token]]) = {
      prefix ~ newr ~~> ((pref, suff) => pref ::: List(suff))
    }

    def delimiter(del: List[CharacterToken]): Rule0 = rule {
      del.foldLeft(EMPTY) { (rule, token) =>
        rule ~ sameAs(token)
      }
    }

    def until(delimiter: Rule0): Rule1[List[Token]] = rule {
      delimiter ~ push(Nil) | argument ~ until(delimiter) ~~> ((a, list) => a ::: list)
    }

    def builder(params: List[Parameter], akk: Rule1[List[List[Token]]]): Rule1[List[List[Token]]] = params match {
      case Left(ParameterToken(_)) :: (next @ Left(ParameterToken(_))) :: rest =>
        // undelimited parameter
        builder(next :: rest, newRule(akk, argument))
      case List(Left(ParameterToken(_))) =>
        // undelimited parameter at the very end of parameter list
        newRule(akk, argument)
      case Left(_) :: Right(delim) :: rest =>
        // delimited parameter
        // read until delimiter is found
        builder(rest, newRule(akk, until(delimiter(delim))))
      case Right(delim) :: rest =>
        // reads and drop the delimiter
        builder(rest, newRule(akk, delimiter(delim) ~ push(Nil)))
      case Nil => akk
    }
    builder(parameters, push(Nil))
  }

  // ================ helper parsers ================

  /* an argument is either a group or single character token */
  protected def argument: Rule1[List[Token]] = rule {
    // braces are dropped from the outermost group
    group ~~> (tokens => tokens.tail.dropRight(1)) |
      (token ~~> (t => List(t)))
  }

}