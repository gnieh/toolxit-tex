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
package toolxit.tex

import scala.util.parsing.combinator.RegexParsers

/**
 *
 * A collection of parsers to parse PdfLaTeX log files.
 *
 * @author Lucas Satabin
 * @author Audric Schiltknecht
 *
 */
object PdfLaTeXLogParser extends RegexParsers {
  /**
   * A pdflatex logfile consists in a list of:
   *  - TODO
   *  - TeX stats
   *  - Output stats
   *  - PDF stats
   */
  lazy val logFile: Parser[PdfLaTeXLog] = {
    rep(outputstat | stuff ^^^ null)
  } ^^ { values => PdfLaTeXLog(values)
  }

  lazy val outputstat: Parser[TeXStatistic] = {
    "Output written on " ~> "\\w+\\.\\w+".r ~ ("(" ~> "\\d+".r <~ "pages, ") ~ "\\d+".r <~ "bytes)." ^^ {
      case output ~ pages ~ bytes=>
        TeXStatistic("pages", pages.toString)
    }
  }

  lazy val stuff: Parser[String] = "[^\n]".r
  /*
  lazy val pdfstats: Parser[TexStatistic] = {
    "PDF statistics:" ~ rep(pdfstat)
  }
  
  lazy val pdfstat: Parser[TexStatistic] = {
  }
 */
}