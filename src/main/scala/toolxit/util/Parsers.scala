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
package toolxit.util

/** Parser combinators implementation based on the paper *Parsec: Direct Style Monadic Parser Combinators For The Real World*
 *
 *  @author Lucas Satabin
 */
trait Parsers[Token] {

  type Pos = StreamPosition[Token]

  /* The type of the parser state must contain (at least) the remaining input and the current position */
  type State <: Input

  /** Computes the next position from the current position and read token */
  protected def nextPos(current: Pos, read: Token): Pos =
    current.next

  /** Creates a state from the `remaining` input and current position `pos` */
  protected def makeState(old: State, remaining: Stream[Token], pos: Pos): State

  /** The result returned by a paser.
   *
   *  @author Lucas Satabin
   */
  sealed abstract class Result[+T] {
    def reply: Reply[T]
    override def toString = reply.toString
  }

  /** A result that actually consumed some tokens in the input stream
   *
   *  @author Lucas Satabin
   */
  class Consumed[T](rep: =>Reply[T]) extends Result[T] {
    // laziness of reply parameter allows for efficient choice implementation
    lazy val reply = rep
  }
  /** Companion object to build and extract data
   *
   *  @author Lucas Satabin
   */
  object Consumed {
    def apply[T](rep: =>Reply[T]) = new Consumed(rep)
    def unapply[T](res: Result[T]): Option[Reply[T]] = res match {
      case cons: Consumed[T] => Some(cons.reply)
      case _                 => None
    }
  }

  /** A result that did not consume any token in input stream
   *
   *  @author Lucas Satabin
   */
  case class Empty[T](reply: Reply[T]) extends Result[T]

  /** A positioned error message with an optional encountered unexpected token and a
   *  list of expected messages.
   *
   *  @author Lucas Satabin
   */
  case class Message(pos: Pos, unexpected: Option[Token], expected: List[String]) {

    override def toString = {
      val found = unexpected map (_.toString) getOrElse "EOI"
      val expect = expected.mkString("expected: ", " or ", "")
      "Error at " + pos + "\n" + pos.longString + "\nfound: " + found + "\n" + expect
    }

  }

  /** An actual parsing result with a (possibly empty) error message
   *
   *  @author Lucas Satabin
   */
  sealed trait Reply[+T] extends Monadic[T, Reply] {
    val message: Message
  }

  /** A parsing result that corresponds to a success
   *
   *  @author Lucas Satabin
   */
  case class Success[T](value: T, rest: State, message: Message) extends Reply[T] {

    def filter(p: T => Boolean): Reply[T] =
      if(p(value))
        this
      else
        Error(message)

    def flatMap[U](f: T => Reply[U]): Reply[U] =
      f(value)

    def map[U](f: T => U): Reply[U] =
      Success(f(value), rest, message)

    override def toString = value.toString
  }

  /** A parsing result that corresponds to an error
   *
   *  @author Lucas Satabin
   */
  case class Error(message: Message) extends Reply[Nothing] {
    def filter(p: Nothing => Boolean): Reply[Nothing] =
      this

    def flatMap[U](f: Nothing => Reply[U]): Reply[U] =
      this

    def map[U](f: Nothing => U): Reply[U] =
      this

    override def toString = message.toString
  }

  /** The input state with input stream and current position
   *
   *  @author Lucas Satabin
   */
  trait Input {
    val stream: Stream[Token]
    val pos: Pos
  }

  /** A monadic parser (function from input to result)
   *
   *  @author Lucas Satabin
   */
  abstract class Parser[+T] extends (State => Result[T]) with Monadic[T, Parser] {
    self =>

    /** Binds the result of this parser to a function producing another parser.
     *  This is the basic operation used to build sequences. */
    def >>=[U](fun: T => Parser[U]): Parser[U] =
      new Parser[U] {
        def apply(input: State): Result[U] = self(input) match {
          case Empty(reply1) => reply1 match {
            case Success(value, rest, msg) =>
              // this parser succeeded without consuming any token
              fun(value)(rest)
            case Error(msg) =>
              // this parser failed to be applied
              Empty(Error(msg))
          }
          case Consumed(reply1) =>
            // this parser consumed some token, immediately return a `Consumed` token
            // with lazy second result parameter
            Consumed(reply1 match {
              case Success(value, rest, _) => fun(value)(rest).reply
              case Error(msg)         => Error(msg)
            })
        }
      }

    /** Applies transformation on the value resulting from this parser. */
    def fmap[U](fun: T => U): Parser[U] =
      new Parser[U] {
        def apply(input: State): Result[U] = self(input) match {
          case Consumed(Success(value, rest, msg)) =>
            Consumed(Success(fun(value), rest, msg))
          case Consumed(Error(msg)) =>
            Consumed(Error(msg))
          case Empty(Success(value, rest, msg)) =>
            Empty(Success(fun(value), rest, msg))
          case Empty(Error(msg)) =>
            Empty(Error(msg))
        }
      }

    /** Synonym for `>>=`, making it possible to use for-comprehension on parsers. */
    @inline
    def flatMap[U](fun: T => Parser[U]): Parser[U] =
      this >>= fun

    /** Synonym for `fmap` making it possible to use for-comprehension on parsers. */
    @inline
    def map[U](fun: T => U): Parser[U] =
      fmap(fun)

    /** Parser that succeeds if `this` parser succeeds and the predicates holds on the
     *  produced value. if `this` parser succeeds but the predicate is not satisfied,
     *  an error result is returned. If `this` failed, the original failing result is
     *  returned */
    def filter(p: T => Boolean): Parser[T] =
      new Parser[T] {
        def apply(input: State): Result[T] = self(input) match {
          case res @ Empty(Success(value, _, _)) if p(value) =>
            res
          case res @ Consumed(Success(value, _, _)) if p(value) =>
            res
          case Empty(Success(_, _, _)) | Consumed(Success(_, _, _)) =>
            Empty(Error(Message(input.pos, input.stream.headOption, Nil)))
          case error =>
            error
        }
      }

    /** Deterministic choice. If this parser fails, the second is tried.
     *  It implements the 'longest match' rule if `this` parser succeeds without consuming
     *  any input, and `that` succeeds by consuming some input, then the second result is taken */
    def <|>[U >: T](that: =>Parser[U]): Parser[U] =
      new Parser[U] {
        def apply(input: State): Result[U] = self(input) match {
          case Empty(Error(msg1)) =>
            that(input) match {
              case Empty(Error(msg2)) =>
                mergeError(msg1, msg2)
              case Empty(Success(token, rest, msg2)) =>
                mergeSuccess(token, rest, msg1, msg2)
              case consumed =>
                consumed
            }
          case Empty(Success(value, rest, msg1)) =>
            that(input) match {
              case Empty(Error(msg2)) =>
                mergeSuccess(value, rest, msg1, msg2)
              case Empty(Success(_, _, msg2)) =>
                mergeSuccess(value, rest, msg1, msg2)
              case consumed =>
                consumed
            }
          case Consumed(Error(msg1)) =>
            that(input) match {
              case Empty(Error(msg2)) =>
                mergeError(msg1, msg2)
              case Empty(Success(token, rest, msg2)) =>
                mergeSuccess(token, rest, msg1, msg2)
              case consumed =>
                consumed
            }
          case consumed =>
            consumed
        }
      }

    /** Parser that accepts the same input as `this` parser, but set the expected productions
     *  to `msg` when `this` fails without consuming any input. */
    def <#>(msg: String): Parser[T] =
      new Parser[T] {
        def apply(input: State): Result[T] = self(input) match {
          case Empty(Error(Message(pos, unexp, _))) =>
            Empty(Error(Message(pos, unexp, List(msg))))
          case res =>
            res
        }
      }

    /** Parser that post processes the state value once `this` parser succeeded.
     *  It does nothing if `this` parser failed */
    def post(f: (T,State) => State): Parser[T] =
      new Parser[T] {
        def apply(input: State): Result[T] = self(input) match {
          case Empty(Success(value, input1, msg)) =>
            Empty(Success(value, f(value, input1), msg))
          case Consumed(Success(value, input1, msg)) =>
            Consumed(Success(value, f(value, input1), msg))
          case error =>
            error
        }
      }

  }

  /** Parser that always succeeds as long as there is at least one token left in the input */
  lazy val any: Parser[Token] =
    new Parser[Token] {
      def apply(input: State): Result[Token] = input.stream match {
        case token #:: rest =>
          Consumed(Success(token, makeState(input, rest, nextPos(input.pos, token)), Message(input.pos, None, Nil)))
        case Stream.Empty =>
          Empty(Error(Message(input.pos, None, List("any token"))))
      }
    }

  /** Parser that always succeeds without consuming any value */
  def success[T](value: T): Parser[T] =
    new Parser[T] {
      def apply(input: State): Result[T] =
        // ok without consuming anything
        Empty(Success(value, input, Message(input.pos, None, Nil)))
    }

  /** Parser that always fails with the given message and wihout consuming any input */
  def fail(msg: String): Parser[Nothing] =
    new Parser[Nothing] {
      def apply(input: State): Result[Nothing] =
        Empty(Error(Message(input.pos, input.stream.headOption, List(msg))))
    }

  /** Parser that succeeds if the next token satisfies the predicate.
   *  The token is consumed when the parser succeeds */
  def satisfy(p: Token => Boolean): Parser[Token] =
    new Parser[Token] {
      def apply(input: State): Result[Token] = input.stream match {
        case token #:: rest if p(token) =>
          val pos1 = nextPos(input.pos, token)
          val input1 = makeState(input, rest, pos1)
          Consumed(Success(token, input1, Message(input.pos, None, Nil)))
        case token #:: rest =>
          // the token does not satisfy the predicate
          Empty(Error(Message(input.pos, Some(token), Nil)))
        case _ =>
          Empty(Error(Message(input.pos, None, Nil)))
      }
    }

  /** Parser that succeeds just like `p`, but pretends that no input was consumed when `p` fails */
  def attempt[T](p: =>Parser[T]): Parser[T] =
    new Parser[T] {
      def apply(input: State): Result[T] = p(input) match {
        case Consumed(Error(msg)) => Empty(Error(msg))
        case res             => res
      }
    }

  /** Parser that succeeds with a list of at least one element recognized by parser `p` */
  def many1[T](p: =>Parser[T]): Parser[List[T]] = {
    lazy val p1 = p
    for {
      x <- p1
      xs <- many1(p1) <|> success(List())
    } yield x :: xs
  }

  /** Parser that succeeds with a list of elements recognized by parser `p` */
  def many[T](p: =>Parser[T]): Parser[List[T]] =
    many1(p) <|> success(List())

  /** Parser that may depend on the current input state */
  def withState[T](p: State => Parser[T]): Parser[T] =
    new Parser[T] {
      def apply(input: State): Result[T] =
        p(input)(input)
    }

  def run[T](p: Parser[T], in: State): Reply[T] = p(in).reply

  // ========== internals ==========

  /* merges both error messages into one error result */
  private  def mergeError(msg1: Message, msg2: Message): Empty[Nothing] =
    Empty(Error(merge(msg1, msg2)))

  /* merges both message as a succesful empty result */
  private def mergeSuccess[T](value: T, rest: State, msg1: Message, msg2: Message): Empty[T] =
    Empty(Success(value, rest, merge(msg1, msg2)))

  private def merge(msg1: Message, msg2: Message) = (msg1, msg2) match {
    case (Message(pos, tok, expected1), Message(_, _, expected2)) =>
      Message(pos, tok, expected1 ++ expected2)
  }


}
