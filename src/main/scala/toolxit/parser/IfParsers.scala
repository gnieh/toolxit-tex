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

/** A bunch of parsers that allows people to declare and use conditional commands
 *
 *  @author Lucas Satabin
 */
trait IfParsers {
  this: TeXParsers =>


  lazy val expandedIf: Parser[Token] =
    attempt(for {
      ControlSequenceToken("else", false) <- any
      st <- getState
      if st.skipElse
      () <- setState {
        // restore including state with the current environment
        // (we may have enter a block...)
        st.including.get.copy(env = st.env)
      }
      // and retry
      tok <- expanded
    } yield tok) <||>
    attempt(ifnum) <||>
    attempt(ifdim) <||>
    attempt(ifodd) <||>
    attempt(ifvmode) <||>
    attempt(ifhmode) <||>
    attempt(ifmmode) <||>
    attempt(ifinner) <||>
    attempt(iftoken) <||>
    attempt(ifcat) <||>
    attempt(ifx)

  lazy val ifnum: Parser[Token] = {
    val preamble = for {
      ControlSequenceToken("ifnum", false) <- any
      number1 <- number
      rel <- relation
      number2 <- number
    } yield Relation(number1, rel, number2)
    expandIf(preamble)
  }

  lazy val ifdim: Parser[Token] = {
    val preamble = for {
      ControlSequenceToken("ifdim", false) <- any
      dimen1 <- dimen
      rel <- relation
      dimen2 <- dimen
    } yield Relation(dimen1, rel, dimen2)
    expandIf(preamble)
  }

  lazy val ifodd: Parser[Token] = {
    val preamble = for {
      ControlSequenceToken("ifodd", false) <- any
      n <- number
    } yield Relation.odd(n)
    expandIf(preamble)
  }

  lazy val ifvmode: Parser[Token] = {
    val preamble = for {
      ControlSequenceToken("ifvmode", false) <- any
      st <- getState
    } yield
      st.env.mode == Mode.VerticalMode ||
      st.env.mode == Mode.InternalVerticalMode
    expandIf(preamble)
  }

  lazy val ifhmode: Parser[Token] = {
    val preamble = for {
      ControlSequenceToken("ifhmode", false) <- any
      st <- getState
    } yield
      st.env.mode == Mode.HorizontalMode ||
      st.env.mode == Mode.RestrictedHorizontalMode
    expandIf(preamble)
  }

  lazy val ifmmode: Parser[Token] = {
    val preamble = for {
      ControlSequenceToken("ifmmode", false) <- any
      st <- getState
    } yield
      st.env.mode == Mode.MathMode ||
      st.env.mode == Mode.DisplayMathMode
    expandIf(preamble)
  }

  lazy val ifinner: Parser[Token] = {
    val preamble = for {
      ControlSequenceToken("ifmmode", false) <- any
      st <- getState
    } yield
      st.env.mode == Mode.InternalVerticalMode ||
      st.env.mode == Mode.RestrictedHorizontalMode ||
      st.env.mode == Mode.MathMode
    expandIf(preamble)
  }

  lazy val iftoken: Parser[Token] = {
    val preamble = for {
      ControlSequenceToken("if", false) <- any
      token1 <- expanded
      token2 <- expanded
    } yield toCharacter(token1).value == toCharacter(token2).value
    expandIf(preamble)
  }

  lazy val ifcat: Parser[Token] = {
    val preamble = for {
      ControlSequenceToken("ifcat", false) <- any
      token1 <- expanded
      token2 <- expanded
    } yield toCharacter(token1).category == toCharacter(token2).category
    expandIf(preamble)
  }

  lazy val ifx: Parser[Token] = {
    val preamble = for {
      ControlSequenceToken("ifx", false) <- any
      token1 <- any
      token2 <- any
      // TODO make it work for control sequences
    } yield token1 == token2
    expandIf(preamble)
  }

  private def toCharacter(token: Token): CharacterToken = token match {
    case c @ CharacterToken(_, _) =>
      c
    case ControlSequenceToken(_, _) =>
      CharacterToken(256, Category.INVALID_CHARACTER)
    case _ =>
      throw new TeXInternalException("This case should never happen")
  }
  private def expandIf(preamble: Parser[Boolean]): Parser[Token] =
    attempt(for {
      // if the condition is true
      true <- preamble
      // set the `skip else` flag in the parser state
      () <- updateState { st =>
        st.copy(skipElse = true, including = Some(st))
      }
      // and retry
      tok <- expanded
    } yield tok) <||>
    (for {
      // if the condition is false
      false <- preamble
      // skip the `then` part (tokens are not expanded in the skipped part)
      () <- skipThen
      // and retry
      tok <- expanded
    } yield tok)

  lazy val relation: Parser[Relation.Value] =
    for {
      CharacterToken(Relation(rel), Category.OTHER_CHARACTER) <- expanded
    } yield rel

  private lazy val skipThen: Parser[Unit] =
    // when skipping, no expansion is performed
    attempt(for {
      ControlSequenceToken("else", false) <- any
    } yield ()) <||>
    attempt(skipIf) <||>
    (for {
      _ <- any
      () <- skipThen
    } yield ())

  private lazy val skipElse: Parser[Unit] =
    attempt(for {
      ControlSequenceToken("fi", false) <- any
    } yield ()) <||>
    attempt(skipIf) <||>
    (for {
      _ <- any
      () <- skipElse
    } yield ())

  private lazy val skipIf: Parser[Unit] =
    // skip a complete \if... \fi block (properly nested)
    // without any expansion
    attempt(for {
      ControlSequenceToken(name, false) <- any
      if name.startsWith("if")
      () <- skipIf // skip nested if
      () <- skipIf // and go until the end of this one
    } yield ()) <||>
    attempt(for {
      // ok, done!
      ControlSequenceToken("fi", false) <- any
    } yield ()) <||>
    (for {
      _ <- any // drop next one
      () <- skipIf // go to end
    } yield ())

}

object Relation extends Enumeration {
  val Lt, Gt, Eq = Value

  def unapply(c: Char): Option[Value] = c match {
    case '<' => Some(Lt)
    case '>' => Some(Gt)
    case '=' => Some(Eq)
    case _   => None
  }

  def apply[T <% Ordered[T]](v1: T, rel: Value, v2: T) = rel match {
    case Lt => v1 < v2
    case Gt => v1 > v2
    case Eq => v1 == v2
  }

  def odd(n: Int) = n % 2 == 1

}

