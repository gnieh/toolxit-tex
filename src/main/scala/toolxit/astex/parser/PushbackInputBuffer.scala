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

import org.parboiled.buffers.InputBuffer
import org.parboiled.support.IndexRange
import org.parboiled.support.Position

import java.util.Arrays

/** An InputBuffer wrapping another InputBuffer and providing the ability to
 *  push back characters on top of the wrapped buffer.
 */
class PushbackInputBuffer(buffer: InputBuffer) extends InputBuffer {
  private var inserts = Array[Int]()
  private var chars = Array[Char]()

  def charAt(index: Int) = {
    val j = Arrays.binarySearch(inserts, index)
    if (j >= 0)
      chars(j)
    else
      buffer.charAt(index + (j + 1))
  }

  def test(index: Int, characters: Array[Char]) = {
    throw new UnsupportedOperationException
  }

  def getPosition(index: Int) = {
    buffer.getPosition(map(index))
  }

  def getOriginalIndex(index: Int) = {
    buffer.getOriginalIndex(map(index))
  }

  def extractLine(lineNumber: Int) = {
    buffer.extractLine(lineNumber)
  }

  def extract(start: Int, end: Int) = {
    buffer.extract(map(start), map(end))
  }

  def extract(range: IndexRange) = {
    buffer.extract(map(range.start), map(range.end))
  }

  def getLineCount = {
    buffer.getLineCount
  }

  private def map(index: Int) = {
    var j = Arrays.binarySearch(inserts, index)
    if (j < 0)
      j = -(j + 1)
    index - j
  }

  //  def pushback()

  def insertChar(index: Int, c: Char) {
    var j = Arrays.binarySearch(inserts, index)
    if (j < 0)
      j = -(j + 1)

    val newChars = new Array[Char](chars.length + 1)
    System.arraycopy(chars, 0, newChars, 0, j)
    newChars(j) = c
    System.arraycopy(chars, j, newChars, j + 1, chars.length - j)
    chars = newChars

    val newInserts = new Array[Int](inserts.length + 1)
    System.arraycopy(inserts, 0, newInserts, 0, j)
    newInserts(j) = index
    for (i <- j until inserts.length) {
      newInserts(i + 1) = inserts(i) + 1
    }
    inserts = newInserts
  }

  def undoCharInsertion(index: Int) = {
    val j = Arrays.binarySearch(inserts, index)
    if (j < 0) throw new Exception("Cannot undo a non-existing insertion")
    val removedChar = chars(j)

    val newChars = new Array[Char](chars.length - 1)
    System.arraycopy(chars, 0, newChars, 0, j)
    System.arraycopy(chars, j + 1, newChars, j, newChars.length - j)
    chars = newChars

    val newInserts = new Array[Int](inserts.length - 1)
    System.arraycopy(inserts, 0, newInserts, 0, j)
    for (i <- (j + 1) until inserts.length) {
      newInserts(i - 1) = inserts(i) - 1
    }
    inserts = newInserts
    removedChar
  }

  def replaceInsertedChar(index: Int, c: Char) {
    val j = Arrays.binarySearch(inserts, index)
    if (j < 0) throw new Exception("Can only replace chars that were previously inserted")
    chars(j) = c
  }
}