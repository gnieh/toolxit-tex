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

object Primitives {
  /** Set of all primitive control sequence names that shall be expanded */
  val expandablePrimitives = Set(
    // all if primitive control sequences
    "ifnum",
    "ifdim",
    "ifodd",
    "ifvmode",
    "ifhmode",
    "ifmmode",
    "ifinner",
    "if",
    "ifcat",
    "ifx",
    "ifvoid",
    "ifeof",
    "iftrue",
    "iffalse",
    "ifcase",
    // a number
    "number",
    // a roman numeral
    "romannumeral",
    // a raw string
    "string",
    // the job name
    "jobname",
    // the external font name
    "fontname",
    // the meaning of a token
    "meaning",
    // start block of a control sequence name
    "csname",
    // expand after the next token
    "expandafter",
    // inhibits expansion of the next token
    "noexpand",
    // mark registers
    "topmark",
    "firstmark",
    "botmark",
    "splitfirstmark",
    "splitbotmark",
    // input file
    "input",
    // end of input file
    "endinput",
    // the magic and power `the` command
    "the")
}

/** Defines and initializes primitive control sequence
 *
 *  @author Lucas Satabin
 *
 */
//trait PrimitiveControlSequences {
//  // it is intended to be mixed in with a TeX environment
//  this: TeXEnvironment =>
//
//  def pdef(stream: Stream[Token]): Stream[Token] = {
//    stream
//  }
//
//  /** The command `\catcode`\{=1' assigns `{' to category 1 */
//  def pcatcode(stream: Stream[Token]): Stream[Token] = {
//    stream.headOption match {
//      case Some(ControlSequenceToken("catcode")) =>
//        var result = stream.drop(1)
//        // catcode then expects a number
//        number(result) match {
//          case (str, Some(char)) =>
//            // some character code was found
//            result = str
//            // then expect an "="
//            result.headOption match {
//              case Some(CharacterToken('=', _)) =>
//                number(result.drop(1)) match {
//                  case (str2, Some(cat)) if cat >= 0 && cat <= 15 =>
//                    //                    println("character " + char.toChar + " now has category code " + cat)
//                    category(char.toChar) = Category(cat)
//                    str2
//                  case _ =>
//                    throw new ControlSequenceException("\\catcode expects a number in range 0..15 as third parameter")
//                }
//              case _ =>
//                throw new ControlSequenceException("\\catcode expects a `=` as second parameter")
//            }
//          case _ =>
//            throw new ControlSequenceException("\\catcode expects a number as first parameter")
//        }
//      case _ => stream
//    }
//  }
//
//  protected def intallPrimitives {
//    css("catcode") = PrimitiveMacro("catcode", pcatcode)
//  }
//
//  // ==== internals ====
//
//  // parses a number on the stream and returns the modified stream with the interpreted number
//  private def number(stream: Stream[Token]) = {
//    // we need a lookahead of 2 tokens
//    val lookahead = stream.take(2).toList
//    lookahead.headOption match {
//      case Some(CharacterToken('`', Category.OTHER_CHARACTER)) if lookahead.size == 2 =>
//        // character internal code
//        lookahead(1) match {
//          case CharacterToken(c, _) =>
//            (stream.drop(2), Some(c.toInt))
//          case ControlSequenceToken(cs) if cs.size == 1 =>
//            // single symbol control sequence name
//            (stream.drop(2), Some(cs(0).toInt))
//          case _ => (stream, None)
//        }
//      case Some(CharacterToken(''', Category.OTHER_CHARACTER)) if lookahead.size == 2 =>
//        // octal number (from '0 to '377)
//        // take all the octal numbers after the ' to build the number
//        var str = stream.drop(1).takeWhile({
//          case CharacterToken(c, _) if c >= 48 && c <= 55 => true
//          case _ => false
//        }).map(_.asInstanceOf[CharacterToken].value - 48).toList
//        val number = str.foldLeft(0) { (res, current) => res * 8 + current }
//        (stream.drop(1 + str.size), Some(number))
//      case Some(CharacterToken('"', Category.OTHER_CHARACTER)) if lookahead.size == 2 =>
//        // hexadecimal number
//        // take all the hexadecimal numbers after the " to build the number
//        var str = stream.drop(1).takeWhile({
//          case CharacterToken(c, _) if (c >= 48 && c <= 57) || (c >= 65 && c <= 70) => true
//          case _ => false
//        }).map(_.asInstanceOf[CharacterToken].value).toList
//        val number = str.foldLeft(0) { (res, current) =>
//          res * 16 + (if (current <= 57) current - 48 else current - 55)
//        }
//        (stream.drop(1 + str.size), Some(number))
//      case Some(CharacterToken(c, _)) if Character.isDigit(c) =>
//        // decimal number
//        // take all the subsequent decimal numbers to build the number
//        var str = stream.takeWhile({
//          case CharacterToken(c, _) if (c >= 48 && c <= 57) => true
//          case _ => false
//        }).map(_.asInstanceOf[CharacterToken].value - 48).toList
//        val number = str.foldLeft(0) { (res, current) =>
//          res * 10 + current
//        }
//        (stream.drop(str.size), Some(number))
//      case _ =>
//        // not a number
//        (stream, None)
//    }
//  }
//
//}