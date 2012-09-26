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

import java.util.Arrays

import org.parboiled.buffers.InputBuffer
import org.parboiled.support._

/** An input buffer based on a (possibly infinite) stream.
 *  The input is lazily read and line information is not available
 *
 *  @author Lucas Satabin
 *
 */
class StreamInputBuffer(private val stream: Stream[Char]) extends InputBuffer {

  def this(generator: => Char) =
    this(Stream.continually(generator).takeWhile(_ != Chars.EOI))

  def charAt(index: Int) = {
    val dropped = stream.drop(index)
    if (dropped.isEmpty) {
      Chars.EOI
    } else {
      dropped.head
    }
  }

  def test(index: Int, characters: Array[Char]) =
    characters.sameElements(stream.drop(index).take(characters.size))

  def extract(start: Int, end: Int) =
    stream.drop(start).take(end - start + 1).mkString

  def extract(range: IndexRange) =
    extract(range.start, range.end)

  def getPosition(index: Int) =
    throw new UnsupportedOperationException("StreamInputBuffer.getPosition")

  def getOriginalIndex(index: Int) = {
    index
  }

  def extractLine(lineNumber: Int) =
    throw new UnsupportedOperationException("StreamInputBuffer.extractLine")

  def getLineCount =
    throw new UnsupportedOperationException("StreamInputBuffer.getLineCount")

  def drop(n: Int) =
    new StreamInputBuffer(stream.drop(n))

}