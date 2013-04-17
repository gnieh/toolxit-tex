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

import gnieh.pp._

import org.scalatest._

class MacroParserTest extends FlatSpec with ShouldMatchers {

  val env = new TeXEnvironment(None)

  val lexer = new TeXLexers with StreamProcessor[Char] {

    protected def createState(input: Stream[Char]): State =
      TeXLexerState(input, StreamPosition(input, 0), ReadingState.N, env)

  }

  val parser = new TeXParsers with StreamProcessor[Token] {

    protected def createState(input: Stream[Token]): State =
      TeXState(input, StreamPosition(input, 0), env)

  }

  val render = new PrettyRenderer(80)

  def pp(token: Token) {
    println(render(token.debug).layout)
  }

  def stringOf(token: Token) =
    token.toString + token.pos

  import parser._

  def stream(in: TeXState): Stream[Token] =
    parser.stream(expanded, in)

  val plainTeXCat = Map(
    '^' -> Category.SUPERSCRIPT,
    '_' -> Category.SUBSCRIPT,
    '{' -> Category.BEGINNING_OF_GROUP,
    '}' -> Category.END_OF_GROUP,
    '#' -> Category.PARAMETER,
    '$' -> Category.MATH_SHIFT,
    '~' -> Category.ACTIVE_CHARACTER
  )

  def withCat(cats: Map[Char, Category.Value])(body: TeXEnvironment => Unit) {
    val env1 = env.enterGroup
    cats foreach {
      case (c, cat) => env1.category(c) = cat
    }
    body(env1)
  }

  def print(stream: Stream[Token]) {
    val doc = stream.foldRight(empty) { (token, acc) =>
      token.debug :|: acc
    }
    println(render(group(doc)).layout)
  }

  def inputOf(env: TeXEnvironment, st: Stream[Token]): TeXState =
    TeXState(st, StreamPosition(st, 0), env)

  def inputOf(env: TeXEnvironment, st: Stream[Char]): lexer.TeXLexerState =
    lexer.TeXLexerState(st, StreamPosition(st, 0), ReadingState.N, env)

  def checkDef(input: Stream[Char], expected: TeXMacro) {
    withCat(plainTeXCat) { env =>
      val tokens = lexer.stream(lexer.token, inputOf(env, input))
      parser.run(texdef, inputOf(env, tokens)) match {
        case Success(res, _, _) => res should be(expected)
        case Error(msg) => fail(msg.toString)
      }
    }
  }

  "parsing a definition with no parameter" should "succeed" in {
    val input = """\def\test{}""".toStream
    val expected = TeXMacro("test", Nil, Nil)
    checkDef(input, expected)
  }

  it should "succeed with properly nested groups in replacement text" in {
    val input = """\def\test{this {is {a} replacement} {text}}""".toStream
    val expected = TeXMacro("test",
      Nil,
      List(
        CharacterToken('t', Category.LETTER),
        CharacterToken('h', Category.LETTER),
        CharacterToken('i', Category.LETTER),
        CharacterToken('s', Category.LETTER),
        CharacterToken(' ', Category.SPACE),
        CharacterToken('{', Category.BEGINNING_OF_GROUP),
        CharacterToken('i', Category.LETTER),
        CharacterToken('s', Category.LETTER),
        CharacterToken(' ', Category.SPACE),
        CharacterToken('{', Category.BEGINNING_OF_GROUP),
        CharacterToken('a', Category.LETTER),
        CharacterToken('}', Category.END_OF_GROUP),
        CharacterToken(' ', Category.SPACE),
        CharacterToken('r', Category.LETTER),
        CharacterToken('e', Category.LETTER),
        CharacterToken('p', Category.LETTER),
        CharacterToken('l', Category.LETTER),
        CharacterToken('a', Category.LETTER),
        CharacterToken('c', Category.LETTER),
        CharacterToken('e', Category.LETTER),
        CharacterToken('m', Category.LETTER),
        CharacterToken('e', Category.LETTER),
        CharacterToken('n', Category.LETTER),
        CharacterToken('t', Category.LETTER),
        CharacterToken('}', Category.END_OF_GROUP),
        CharacterToken(' ', Category.SPACE),
        CharacterToken('{', Category.BEGINNING_OF_GROUP),
        CharacterToken('t', Category.LETTER),
        CharacterToken('e', Category.LETTER),
        CharacterToken('x', Category.LETTER),
        CharacterToken('t', Category.LETTER),
        CharacterToken('}', Category.END_OF_GROUP)
      )
    )
    checkDef(input, expected)
  }

  "a parameter list" should "be recognized when it consists only in one parameter" in {
    val input = """\def\test#1{}""".toStream
    val expected = TeXMacro("test", List(Left(ParameterToken(1))), Nil)
    checkDef(input, expected)
  }

  "a reference to a parameter" should "be recognized if the parameter exists" in {
    val input = """\def\test#1{#1}""".toStream
    val expected = TeXMacro("test", List(Left(ParameterToken(1))), List(ParameterToken(1)))
    checkDef(input, expected)
  }

  "a complex parameter list" should "be parsed correctly" in {
    val input = """\def\cs AB#1#2C$#3\$ {#3{ab#1}#1 c##\x #2}""".toStream
    val expected = TeXMacro("cs",
      List(
        Right(List(
          CharacterToken('A', Category.LETTER),
          CharacterToken('B', Category.LETTER)
        )),
        Left(ParameterToken(1)),
        Left(ParameterToken(2)),
        Right(List(
          CharacterToken('C', Category.LETTER),
          CharacterToken('$', Category.MATH_SHIFT)
        )),
        Left(ParameterToken(3)),
        Right(List(
          ControlSequenceToken("$")
        ))
      ),
      List(
        ParameterToken(3),
        CharacterToken('{', Category.BEGINNING_OF_GROUP),
        CharacterToken('a', Category.LETTER),
        CharacterToken('b', Category.LETTER),
        ParameterToken(1),
        CharacterToken('}', Category.END_OF_GROUP),
        ParameterToken(1),
        CharacterToken(' ', Category.SPACE),
        CharacterToken('c', Category.LETTER),
        CharacterToken('#', Category.PARAMETER),
        ControlSequenceToken("x"),
        ParameterToken(2)
      )
    )
    checkDef(input, expected)
  }

  "an unknown parameter number" should "be reported as an error" in {
    val input = """\def\toto{#1}""".toStream
    withCat(plainTeXCat) { env =>
      val tokens = lexer.stream(lexer.token, inputOf(env, input))
      parser.run(texdef, inputOf(env, tokens)) match {
        case Success(_, _, _) =>
          fail("an error should have been encountered")
        case Error(UserMessage(pos, msg)) =>
        case Error(msg) =>
          fail(msg.toString)
      }
    }
  }

  "special '#' character" should "be correctly handled in replacement text" in {
    val input = """\def\test{\def\test2##1{a test ##1}}""".toStream
    val expected = TeXMacro("test", Nil,
      List(
        ControlSequenceToken("def"),
        ControlSequenceToken("test"),
        CharacterToken('2', Category.OTHER_CHARACTER),
        CharacterToken('#', Category.PARAMETER),
        CharacterToken('1', Category.OTHER_CHARACTER),
        CharacterToken('{', Category.BEGINNING_OF_GROUP),
        CharacterToken('a', Category.LETTER),
        CharacterToken(' ', Category.SPACE),
        CharacterToken('t', Category.LETTER),
        CharacterToken('e', Category.LETTER),
        CharacterToken('s', Category.LETTER),
        CharacterToken('t', Category.LETTER),
        CharacterToken(' ', Category.SPACE),
        CharacterToken('#', Category.PARAMETER),
        CharacterToken('1', Category.OTHER_CHARACTER),
        CharacterToken('}', Category.END_OF_GROUP)
      )
    )
    checkDef(input, expected)
  }

  ignore should "be correctly interpreted if the last parameter is '#' immediately followed by '{'" in {
    val input = """\def\test#1c#{}""".toStream
    val expected = TeXMacro("test",
      List(
        Left(ParameterToken(1)),
        Right(List(
          CharacterToken('c', Category.LETTER),
          CharacterToken('#', Category.PARAMETER)
        ))
      ),
      Nil
    )
    checkDef(input, expected)
  }

}
