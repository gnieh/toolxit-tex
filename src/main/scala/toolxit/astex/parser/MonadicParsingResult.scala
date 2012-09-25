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

object MonadicParsingResult {
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

/** @author Lucas Satabin
 *
 */
sealed trait MonadicParsingResult[+T] {

  val matched: Boolean

  def get: T

  val parseErrors: List[ParseError]

  def hasErrors: Boolean = parseErrors.nonEmpty

  def map[U](f: T => U): MonadicParsingResult[U] =
    if (matched) OkResult(f(get)) else FailedResult(parseErrors)

  def flatMap[U](f: T => MonadicParsingResult[U]): MonadicParsingResult[U] =
    if (matched) f(get) else FailedResult(parseErrors)

}

final case class OkResult[T](value: T) extends MonadicParsingResult[T] {

  val matched = true

  def get = value

  val parseErrors = Nil

}

final case class FailedResult(parseErrors: List[ParseError]) extends MonadicParsingResult[Nothing] {

  val matched = false

  def get = throw new NoSuchElementException("FailedResult.get")

}

case object EmptyResult extends MonadicParsingResult[Nothing] {

  val matched = true

  def get = throw new NoSuchElementException("EmptyResult.get")

  val parseErrors = Nil
}