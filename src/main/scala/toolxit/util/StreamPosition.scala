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

import scala.util.parsing.input.Position

/** A position that has no context in the stream for what was read before and
 *  what comes next. It is only aware of the current token
 *
 *  @author Lucas Satabin
 */
case class StreamPosition[T](source: Stream[T], offset: Int) extends Position {

  val line = 1

  val column = offset + 1

  def lineContents: String =
    source.headOption.map(_.toString).getOrElse("<EOI>")

  def next: StreamPosition[T] =
    StreamPosition(source.tail, offset + 1)

  /** Returns a string representation of the `Position`, of the form `line.column`. */
  override def toString = "@" + offset

  override def <(that: Position) = that match {
    case StreamPosition(_, that_offset) =>
      this.offset < that_offset
    case _ =>
      super.<(that)
  }
}
