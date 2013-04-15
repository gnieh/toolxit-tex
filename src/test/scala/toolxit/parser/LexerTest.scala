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

import org.scalatest._

class LexerTest extends FlatSpec with ShouldMatchers {

  val env = new TeXEnvironment(None)

  val lexer = new TeXLexers[StreamPosition[Char]] with StreamProcessor[Char, StreamPosition[Char]] {

    protected def createState(input: Stream[Char]): State =
      TeXLexerState(input, StreamPosition(input, 0), ReadingState.N, env)

    protected def nextPos(current: StreamPosition[Char], read: Char): StreamPosition[Char] =
      current.next
  }

  def stringOf(token: Token) =
    token.toString + token.pos

  import lexer._

  def stream(in: TeXLexerState): Stream[Token] =
    lexer.stream(token, in)

  val basicCategories = Map(
    '^' -> Category.SUPERSCRIPT
  )

  def withCat(cats: Map[Char, Category.Value])(body: TeXEnvironment => Unit) {
    val env1 = env.enterGroup
    cats foreach {
      case (c, cat) => env1.category(c) = cat
    }
    body(env1)
  }

  def print(stream: Stream[Token]) {
    println(stream.mkString("\n"))
  }

  def inputOf(env: TeXEnvironment, st: Stream[Char]): TeXLexerState =
    TeXLexerState(st, StreamPosition(st, 0), ReadingState.N, env)

  "a text stream" should "be parsed as a stream of character tokens" in {
    val input = "this is a simple text".toStream
    val expected = input.map { c =>
      val cat = if(c == ' ') Category.SPACE else Category.LETTER
      CharacterToken(c, cat)
    }
    stream(inputOf(env, input)) should be(expected)
  }

  "a control sequence" should "be recognized as such" in {
    val input = "a \\test".toStream
    val expected =
      Stream(CharacterToken('a', Category.LETTER),
        CharacterToken(' ', Category.SPACE),
        ControlSequenceToken("test"))
    stream(inputOf(env, input)) should be(expected)
  }

  it should "be recognized with single non letter character" in {
    val input = "a \\'".toStream
    val expected =
      Stream(CharacterToken('a', Category.LETTER),
        CharacterToken(' ', Category.SPACE),
        ControlSequenceToken("'"))
    stream(inputOf(env, input)) should be(expected)
  }

  "a special character in input stream" should "be transformed by preprocessor" in {
    val input = "^^41".toStream
    val expected = Stream(CharacterToken('A', Category.LETTER))
    withCat(basicCategories) { env =>
      stream(inputOf(env, input)) should be(expected)
    }
  }

  it should "also be recognized when used with a letter" in {
    val input = "^^r".toStream
    val expected = Stream(CharacterToken('2', Category.OTHER_CHARACTER))
    withCat(basicCategories) { env =>
      stream(inputOf(env, input)) should be(expected)
    }
  }

  it should "not be recognized if '^' has not category superscript" in {
    val input = "^^41".toStream
    val expected = Stream(
      CharacterToken('^', Category.OTHER_CHARACTER),
      CharacterToken('^', Category.OTHER_CHARACTER),
      CharacterToken('4', Category.OTHER_CHARACTER),
      CharacterToken('1', Category.OTHER_CHARACTER))
    stream(inputOf(env, input)) should be(expected)
  }

  it should "be considered as a letter as part of a control sequence name" in {
    val input = "\\^^41BC".toStream
    val expected = Stream(ControlSequenceToken("ABC"))
    withCat(basicCategories) { env =>
      stream(inputOf(env, input)) should be(expected)
    }
  }

}
