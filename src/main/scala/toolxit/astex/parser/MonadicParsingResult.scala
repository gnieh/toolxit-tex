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
package toolxit.astex.parser

import org.parboiled.support.ParsingResult
import org.parboiled.errors.ParseError

import java.util.NoSuchElementException

import scala.collection.JavaConverters._

/** Companion object used to create [[toolxit.astex.parser.MonadicParsingResult]]s. */
object MonadicParsingResult {
  /** Creates a [[toolxit.astex.parser.MonadicParsingResult]] from a `ParsingResult` with
   *  following rules:
   *   - if `result` is `null`, [[toolxit.astex.parser.EmptyResult]] is returned
   *   - if `result` matched, a [[toolxit.astex.parser.OkResult]] is returned
   *   - if `result` had errors, a [[toolxit.astex.parser.FailedResult]] is returned
   */
  def apply[T](result: ParsingResult[T]): MonadicParsingResult[T] = result match {
    case null =>
      // end of input reached
      EmptyResult
    case result if result.matched =>
      // a token was successfully parsed
      OkResult(result.resultValue)
    case result =>
      // there was a parse error
      FailedResult(result.parseErrors.asScala.toList)
  }
}

/** A monadic parsing result, exposes methods to work with the (possibly absent) result
 *  of a parser run, and to manage situations when there were parse errors or no result
 *  at all.
 *
 *  @author Lucas Satabin
 *
 */
sealed trait MonadicParsingResult[+T] {

  /** Did the parser run matched? */
  val matched: Boolean

  /** The matched value. */
  def get: T

  /** The (possibly empty) list of parse errors. */
  val parseErrors: List[ParseError]

  /** Did the parser run generate error(s)? */
  def hasErrors: Boolean = parseErrors.nonEmpty

  /** Is the result empty? */
  def isEmpty: Boolean

  /** Is the result non empty? */
  def nonEmpty: Boolean = !isEmpty

  /** Transforms the returned value according to the transformation function. */
  def map[U](f: T => U): MonadicParsingResult[U] =
    if (matched && nonEmpty) OkResult(f(get))
    else if (isEmpty) EmptyResult
    else FailedResult(parseErrors)

  /** Transforms the returned value according to the transformation function. */
  def flatMap[U](f: T => MonadicParsingResult[U]): MonadicParsingResult[U] =
    if (matched && nonEmpty) f(get)
    else if (isEmpty) EmptyResult
    else FailedResult(parseErrors)

}

/** A non empty monadic parsing result (successful or fail). */
sealed trait NonEmptyMonadicParsingResult[+T]
    extends MonadicParsingResult[T] {
  def isEmpty = false
}

/** Companion object exposing extractor for non empty monadic results. */
object NonEmptyMonadicParsingResult {
  def unapply[T](result: MonadicParsingResult[T]): Option[NonEmptyMonadicParsingResult[T]] =
    result match {
      case EmptyResult => None
      case r @ OkResult(_) => Some(r)
      case r: FailedResult => Some(r)
    }
}

/** The parser run returned a matched value. */
final case class OkResult[T](value: T)
    extends NonEmptyMonadicParsingResult[T] {

  val matched = true

  def get = value

  val parseErrors = Nil

}

/** The parser run generated some parse errors. */
final case class FailedResult(parseErrors: List[ParseError])
    extends NonEmptyMonadicParsingResult[Nothing] {

  val matched = false

  def get = throw new NoSuchElementException("FailedResult.get")

}

/** The parser run returned no result (probably EOI reached). */
case object EmptyResult
    extends MonadicParsingResult[Nothing] {

  val matched = true

  def isEmpty = true

  def get = throw new NoSuchElementException("EmptyResult.get")

  val parseErrors = Nil
}