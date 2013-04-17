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

import util._

/** A bunch of parsers that define TeX element such as
 *   - user macros
 *   - counters
 *   - fonts
 *   - dimensions
 *   - ...
 *
 *  @author Lucas Satabin
 */
trait TeXDefinitionParsers extends Parsers[Token] {
  this: TeXParsers =>

  lazy val modifier: Parser[Modifier.Value] =
    (for (_ <- controlSequence("global")) yield Modifier.Global) <|>
    (for (_ <- controlSequence("long")) yield Modifier.Long) <|>
    (for (_ <- controlSequence("outer")) yield Modifier.Outer)

  lazy val texdef: Parser[TeXMacro] =
    for {
      // \def
      _ <- controlSequence("def")
      // \name
      ControlSequenceToken(name) <- controlSequence
      // <parameter text>
      params <- parameterText
      // {
      _ <- beginningOfGroup
      // disable macro expansion, and reinit group nesting
      st <- getState
      () <- setState(st.copy(expansion = false, currentNesting = 0))
      // <replacement text>
      replacement <- replacementText
      // }
      _ <- endOfGroup
      // reenable macro expansion
      () <- setState(st)
    } yield TeXMacro(name, params, replacement)

  /** the parameter text is composed of either delimited or undelimited parameters.
   *  the current parameter counter is reset to 0 before parsing the parameters so that
   *  errors can be reported */
  lazy val parameterText: Parser[List[Parameter]] =
    (for {
      // reinitialize parameter number
      () <- updateState(st => st.copy(currentParam = 0))
      params <- until(someParameter, beginningOfGroup)
    } yield params)

  /** either a delimited parameter or an undelimited parameter */
  lazy val someParameter: Parser[Parameter] =
    (for {
      (p @ ParameterToken(n)) <- param
      st <- getState
      if st.currentParam == n - 1
      () <- updateState(st => st.copy(currentParam = n))
    } yield Left(p)) <|>
    (for {
      ParameterToken(n) <- param
      st <- getState
      f <- fail("Parameters must be numbered consecutively. The next parameter number should be " + (st.currentParam + 1) + " and not " + n)
    } yield f) <|>
    (for {
      chars <- until(next, parameter <|> beginningOfGroup)
    } yield Right(chars))

  /** tokens that make the replacement text */
  lazy val replacementText: Parser[List[Token]] =
    until(
      // either `##` for character token `#`
      (for {
        _ <- parameter
        p <- parameter
      } yield p) <|>
      // or `#n` where n is a valid parameter number for the current macro
      (for {
        // accept parameters only if it exists in current context
        (p @ ParameterToken(n)) <- param
        st <- getState
        if n > 0 && st.currentParam >= n
      } yield p) <|>
      // or an error
      (for {
        ParameterToken(n) <- param
        st <- getState
        f <- fail("Parameter number " + n + " does not exist in current macro")
      } yield f) <|>
      // or a control sequence
      controlSequence <|>
      // or entering a group
      (for {
        b <- beginningOfGroup
        () <- updateState(st => st.copy(currentNesting = st.currentNesting + 1))
      } yield b) <|>
      // or leaving a group
      (for {
        e <- endOfGroup
        () <- updateState(st => st.copy(currentNesting = st.currentNesting - 1))
      } yield e) <|>
      // or any character other than paremter and end of group
      (for {
        (c @ CharacterToken(_, cat)) <- character
        if cat != Category.PARAMETER && cat != Category.END_OF_GROUP
      } yield c),
      // until end of group at level 0 is reached
      for {
        _ <- endOfGroup
        st <- getState
        if st.currentNesting == 0
      } yield ()
    )

}
