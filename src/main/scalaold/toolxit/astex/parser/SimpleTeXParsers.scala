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

import org.parboiled.scala._

/** A collection of simple basic parsers for basic TeX elements.
 *
 *  @author Lucas Satabin
 */
class SimpleTeXParsers extends TokenParsers {

  import environment._

  def spaces: Rule0 = rule {
    zeroOrMore(whitespace)
  }

  def plusOrMinus = rule {
    sameAs(CharacterToken('+', Category.OTHER_CHARACTER)) ~ push(1) |
      sameAs(CharacterToken('-', Category.OTHER_CHARACTER)) ~ push(-1)
  }

  def optSigns = rule {
    zeroOrMore(plusOrMinus) ~~> { signs =>
      signs.foldLeft(1) { (res, cur) => res * cur }
    }
  }

  def number: Rule1[Int] = rule {
    spaces ~ optSigns ~ spaces ~ unsignedNumber ~~> ((sign, value) => sign * value)
  }

  def number15: Rule1[Short] = rule {
    number ~~~? (value => value >= 0 && value < 32768) ~~> (_.toShort)
  }

  def number8: Rule1[Byte] = rule {
    number ~~~? (value => value >= 0 && value < 256) ~~> (_.toByte)
  }

  def number4: Rule1[Byte] = rule {
    number ~~~? (value => value >= 0 && value < 16) ~~> (_.toByte)
  }

  def unsignedNumber: Rule1[Int] = rule {
    (integerConstant |
      sameAs(CharacterToken(''', Category.OTHER_CHARACTER)) ~ octalConstant |
      sameAs(CharacterToken('"', Category.OTHER_CHARACTER)) ~ hexadecimalConstant |
      sameAs(CharacterToken('`', Category.OTHER_CHARACTER)) ~ characterToken) ~~> (t => 0) ~ optional(whitespace)
  }

  def integerConstant: Rule1[Int] = rule {
    oneOrMore(digit) ~~> { chars =>
      Integer.parseInt(chars.mkString, 10)
    }
  }

  def octalConstant: Rule1[Int] = rule {
    oneOrMore(octalDigit) ~~> { chars =>
      Integer.parseInt(chars.mkString, 8)
    }
  }

  def hexadecimalConstant: Rule1[Int] = rule {
    oneOrMore(hexadecimalDigit) ~~> { chars =>
      Integer.parseInt(chars.mkString, 16)
    }
  }

  def octalDigit: Rule1[Char] = rule {
    ("0" - "7") ~> (_.charAt(0)) ~~~? (c => category(c) == Category.OTHER_CHARACTER)
  }

  def digit: Rule1[Char] = rule {
    ("0" - "9") ~> (_.charAt(0)) ~~~? (c => category(c) == Category.OTHER_CHARACTER)
  }

  def hexadecimalDigit: Rule1[Char] = rule {
    ("0" - "9" | "A" - "F") ~> (_.charAt(0)) ~~~% (c =>
      category(c) == Category.OTHER_CHARACTER ||
        category(c) == Category.LETTER)
  }

  def characterToken: Rule1[Token] = rule {
    character | controlsequence ~~~? { _.name.size == 1 }
  }

}