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

import dimen._

import org.parboiled.scala._

/** These parsers are used to parse TeX primitive types used by commands.
 *  These types are described in chapter 24 of ''The TeX Book''.
 *
 *  @author Lucas Satabin
 *
 */
trait PrimitiveTypeParsers extends TeXParser {
  this: CharacterParsers =>

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

  def number15: Rule1[Int] = rule {
    number ~~~% (value => value >= 0 && value < 32768)
  }

  def number8: Rule1[Int] = rule {
    number ~~~% (value => value >= 0 && value < 256)
  }

  def number4: Rule1[Int] = rule {
    number ~~~% (value => value >= 0 && value < 16)
  }

  def unsignedNumber: Rule1[Int] = rule {
    normalInteger | coercedInteger
  }

  def normalInteger: Rule1[Int] = rule {
    internalInteger |
      (integerConstant |
        sameAs(CharacterToken(''', Category.OTHER_CHARACTER)) ~ octalConstant |
        sameAs(CharacterToken('"', Category.OTHER_CHARACTER)) ~ hexadecimalConstant |
        sameAs(CharacterToken('`', Category.OTHER_CHARACTER)) ~ characterToken) ~ optional(whitespace)
  }

  def integerConstant = rule {
    oneOrMore(digit)
  }

  def octalConstant = rule {
    oneOrMore(octalDigit)
  }

  def hexadecimalConstant = rule {
    oneOrMore(hexadecimalDigit)
  }

  def octalDigit = rule {
    ("0" - "7") ~> (_.charAt(0)) ~~~% (c => category(c) == Category.OTHER_CHARACTER)
  }

  def digit = rule {
    ("0" - "9") ~> (_.charAt(0)) ~~~% (c => category(c) == Category.OTHER_CHARACTER)
  }

  def hexadecimalDigit = rule {
    ("0" - "9" | "A" - "F") ~> (_.charAt(0)) ~~~% (c =>
      category(c) == Category.OTHER_CHARACTER ||
        category(c) == Category.LETTER)
  }

  def characterToken: Rule1[Token] = rule {
    character | controlsequence ~~~? { _.name.size == 1 }
  }

  def coercedInteger = rule {
    internalDimen | internalGlue
  }

  def dimen: Rule1[Dimension] = rule {
    optSigns ~ unsignedDimen ~~> ((sign, dim) => sign * dim)
  }

  def unsignedDimen: Rule1[Dimension] = rule {
    normalDimen | coercedDimen
  }

  def coercedDimen: Rule1[Dimension] = rule {
    internalGlue ~~> (g => g.value)
  }

  def normalDimen: Rule1[Dimension] = rule {
    internalDimen |
      factor ~ unitOfMeasure ~~> Dimension
  }

  def factor: Rule1[Float] = rule {
    normalInteger |
      decimalConstant
  }

  def decimalConstant: Rule1[Float] = rule {
    zeroOrMore(digit) ~
      (sameAs(CharacterToken('.', Category.OTHER_CHARACTER)) |
        sameAs(CharacterToken(',', Category.OTHER_CHARACTER))) ~ zeroOrMore(digit) ~~> {
          (pre, post) =>
            val prePart = pre.foldLeft(0f) { (res, cur) => res * 10 + cur }
            val postPart = post.zipWithIndex.foldLeft(0f) {
              case (res, (cur, idx)) => res + cur / (10 * (idx + 1))
            }
            prePart + postPart
        }
  }

  def unitOfMeasure: Rule1[TeXUnit] = rule {
    (spaces ~ internalUnit) |
      (optional(keyword("true")) ~ physicalUnit ~ optional(whitespace))
  }

  def internalUnit: Rule1[InternalUnit] = rule {
    (keyword("em") ~ push(Em) | keyword("ex") ~ push(Ex)) ~ optional(whitespace)
    internalInteger | internalDimen | internalGlue
  }

  def physicalUnit: Rule1[PhysicalUnit] = rule {
    keyword("pt") ~ push(Point) |
      keyword("pc") ~ push(Pica) |
      keyword("in") ~ push(Inch) |
      keyword("bp") ~ push(BigPoint) |
      keyword("cm") ~ push(Centimeter) |
      keyword("mm") ~ push(Millimeter) |
      keyword("dd") ~ push(DidotPoint) |
      keyword("cc") ~ push(Cicero) |
      keyword("sp") ~ push(ScaledPoint)
  }

  def mudimen = rule {
    optSigns ~ unsignedMudimen
  }

  def unsignedMudimen = rule {
    normalMudimen ~~% { (_, _) => } |
      coercedMudimen ~~% { _ => }
  }

  def coercedMudimen = rule {
    internalMuglue
  }

  def normalMudimen = rule {
    factor ~ muUnit
  }

  def muUnit = rule {
    spaces ~ internalMuglue | keyword("mu") ~ push(MathUnits) ~ optional(whitespace)
  }

  def glue: Rule1[Glue] = rule {
    optSigns ~ internalGlue ~~> { (sign, glue) =>
      glue match {
        case Glue(dim, stretch, shrink) =>
          Glue(sign * dim, sign * stretch, sign * shrink)
      }
    } |
      dimen ~ stretch ~ shrink ~~> Glue
  }

  def stretch: Rule1[Dimension] = rule {
    keyword("plus") ~ dimen |
      keyword("plus") ~ filDimen |
      spaces ~ push(ZeroDimen)
  }

  def shrink: Rule1[Dimension] = rule {
    keyword("minus") ~ dimen |
      keyword("minus") ~ filDimen |
      spaces ~ push(ZeroDimen)
  }

  def filDimen: Rule1[Dimension] = rule {
    optSigns ~ factor ~ filUnit ~ spaces ~~> { (sign, fact, fil) =>
      Dimension(sign * fact, fil)
    }
  }

  def filUnit: Rule1[TeXUnit] = rule {
    keyword("fil") ~ push(Fil) |
      keyword("fill") ~ push(Fill) |
      keyword("filll") ~ push(Filll)
  }

  def muglue = rule {
    optSigns ~ internalMuglue |
      mudimen ~ muStretch ~ muShrink
  }

  def muStretch = rule {
    keyword("plus") ~ mudimen |
      keyword("plus") ~ filDimen |
      spaces
  }

  def muShrink = rule {
    keyword("minus") ~ mudimen |
      keyword("minus") ~ filDimen |
      spaces
  }

  def internalInteger: Rule1[Int] = rule {
    integerParameter ~~> { cs =>
      css(cs.name) match {
        case Some(PrimitiveCounter(_, value)) =>
          value
        case _ => // shall never happen
          throw new TeXException("control sequence " + cs.name + "should refer to a primitive integer register")
      }
    } |
      specialInteger |
      cs("lastpenalty") |
      countdefToken |
      cs("count") ~ number8 ~~% { _ => } |
      codename ~ number8 ~~% { _ => } |
      chardefToken |
      mathchardefToken |
      cs("parshape") |
      cs("inputlineno") |
      cs("hyphenchar") ~ font ~~% { _ => } |
      cs("skewchar") ~ font ~~% { _ => } |
      cs("badness")
  }

  def specialInteger = rule {
    cs("spacefactor") |
      cs("prevgraf") |
      cs("deadcycles") |
      cs("insertpenalties")
  }

  def codename = rule {
    cs("catcode") | cs("mathcode") | cs("lccode") | cs("uccode") | cs("sfcode") | cs("delcode")
  }

  def font = rule {
    fontdefToken |
      cs("font") |
      familyMember ~~% { _ => }
  }

  def familyMember = rule {
    fontRange ~ number4
  }

  def fontRange = rule {
    cs("textfont") | cs("scriptfont") | cs("scriptscriptfont")
  }

  def internalDimen = rule {
    dimenParameter |
      specialDimen |
      cs("lastkern") |
      dimendefToken |
      cs("dimen") ~ number8 ~~% { _ => } |
      boxDimen ~ number8 ~~% { _ => } |
      cs("fontdimen") ~ number ~ font ~~% { (_, _) => }
  }

  def specialDimen = rule {
    cs("prevdepth") | cs("pagegoal") | cs("pagetotal") |
      cs("pagestretch") | cs("pagefilstretch") | cs("pagefillstretch") |
      cs("pagefilllstretch") | cs("pageshrink") | cs("pagedepth")

  }

  def boxDimen = rule {
    cs("ht") | cs("wd") | cs("dp")
  }

  def internalGlue: Rule1[Glue] = rule {
    controlsequence ~~> { cs =>
      css(cs.name) match {
        case Some(TeXGlue(_, glue)) => glue
        case Some(cs) => throw new TeXException(cs.name + " should be of type Glue but has type " + cs.tpe)
        case None => throw new TeXException(cs.name + " does is unknown in the current environment")
      }
    }
  }

  def internalMuglue: Rule1[Muglue] = rule {
    controlsequence ~~> { cs =>
      css(cs.name) match {
        case Some(TeXMuglue(_, muglue)) => muglue
        case Some(TeXGlue("lastskip", glue)) =>
        case Some(cs) => throw new TeXException(cs.name + " should be of type Glue but has type " + cs.tpe)
        case None => throw new TeXException(cs.name + " does is unknown in the current environment")
      }
    }
  }

  // ================ user defined tokens ================

  def countdefToken = rule {
    // control sequence defined with \countdef
    controlsequence ~~~? (cs => css(cs.name) match {
      case Some(_: TeXInteger) => true
      case None => false
    })
  }

  def dimendefToken = rule {
    // control sequence defined with \dimendef
    controlsequence ~~~? (cs => css(cs.name) match {
      case Some(_: TeXDimension) => true
      case None => false
    })
  }

  def chardefToken = rule {
    // control sequence defined with \chardef
    controlsequence ~~~? (cs => css(cs.name) match {
      case Some(_: TeXChar) => true
      case None => false
    })
  }

  def mathchardefToken = rule {
    // control sequence defined with \mathchardef
    controlsequence ~~~? (cs => css(cs.name) match {
      case Some(_: TeXMathChar) => true
      case None => false
    })
  }

  def fontdefToken = rule {
    // control sequence defined with \font
    controlsequence ~~~? (cs => css(cs.name) match {
      case Some(_: TeXFont) => true
      case None => false
    })
  }

  def skipdefToken = rule {
    // control sequence defined with \skipdef
    controlsequence ~~~? (cs => css(cs.name) match {
      case Some(_: TeXGlue) => true
      case None => false
    })
  }

  def muskipdefToken = rule {
    // control sequence defined with \muskipdef
    controlsequence ~~~? (cs => css(cs.name) match {
      case Some(_: TeXMuglue) => true
      case None => false
    })
  }

  // ================ parameter parsers ================

  def integerParameter = rule {
    controlsequence ~~~? (cs => Primitives.integerParameter.contains(cs.name))
  }

  def dimenParameter = rule {
    controlsequence ~~~? (cs => Primitives.dimenParameter.contains(cs.name))
  }

  def glueParameter = rule {
    controlsequence ~~~? (cs => Primitives.glueParameter.contains(cs.name))
  }

  def muglueParameter = rule {
    controlsequence ~~~? (cs => Primitives.muglueParameter.contains(cs.name))
  }

  def tokenParameter = rule {
    controlsequence ~~~? (cs => Primitives.tokenParameter.contains(cs.name))
  }

  // ================ internals ================

  /* a parser for a given keyword */
  private def keyword(name: String) = rule {
    def aLetter(c: Char) = {
      // a parser that matches either lower or upper case of this 
      // character (if this character is not active)
      (ch(c) | ch(c.toUpper)) ~> (_.charAt(0)) ~~?
        (c => category(c) != Category.ACTIVE_CHARACTER)
    }
    name.foldLeft(spaces)((result, current) => result ~ aLetter(current))
  }

  private def cs(name: String) = rule {
    controlsequence ~~~? (cs => cs.name == name)
  }

}