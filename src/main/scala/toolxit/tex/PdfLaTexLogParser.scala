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
   *  - List of files loaded
   *  - Output stats
   */
  lazy val logFile: Parser[PdfLaTeXLog] =
    rep(file | outputstat | stuff ^^^ null) ^^ { entries =>
      val grouped = entries.filter(_ != null).groupBy {
        case _: TexCatalogFile => "catalog"
        case _: Statistic => "stats"
      }
      PdfLaTeXLog(grouped.getOrElse("catalog", Nil).map(_.asInstanceOf[TexCatalogFile]),
        grouped.getOrElse("stats", Nil).map(_.asInstanceOf[Statistic]))
    }

  lazy val file: Parser[TexCatalogFile] =
    /** Accept following formats:
     * \li Simple catalog inclusion: 
     * (/usr/share/texmf-dist/tex/latex/oberdiek/kvoptions.sty
     *   Package: kvoptions 2010/12/23 v3.10 Keyval support for LaTeX options (HO)
     * )
     * 
     * \li Recursive catalog inclusion: 
     * (/usr/share/texmf-dist/tex/latex/base/fontenc.sty
     * Package: fontenc 2005/09/27 v1.99g Standard LaTeX package
     * (/usr/share/texmf-dist/tex/latex/base/t1enc.def
	 * File: t1enc.def 2005/09/27 v1.99g Standard LaTeX file
     * LaTeX Font Info:    Redeclaring font encoding T1 on input line 43.
     * ))
     * 
     * \li Single file inclusion:
     * (/usr/share/texmf-dist/tex/latex/oberdiek/kvoptions.sty)
     */
    ("(" ~> "[\\w-:/\\ .]+".r) ~ rep(description | stuffNoParen ^^^ null) ~ (rep(stuffNoParen) ~> "\\s*".r ~> rep(file | stuffNoParen ^^^ null) <~ "\\s*".r <~ rep(stuffNoParen) <~ ")") ^^ {
      case path ~ descriptions ~ files => Console.println("TC " + path); TexCatalogFile(path, descriptions.filter(_ != null), files.filter(_ != null))
    }

  lazy val description: Parser[TexCatalog] =
    /** Accept following format:
     * Document Class: report 2007/10/19 v1.4h Standard LaTeX document class
     */
    ("[a-zA-Z ]+".r <~ ":") ~ "[\\w.]+".r ~ "\\d{4}/\\d{2}/\\d{2}".r ~ ("v" ~> "[^\\s]+".r) ~ opt(".*$".r) ^^ {
      case catalogType ~ name ~ date ~ version ~ info => catalogType match {
        case "Package" => Console.println("P " + List(name, date, version, info).mkString(" ")); Package(name, date, version, info)
        case "Document Class" => Console.println("C " + List(name, date, version, info).mkString(" ")); Class(name, date, version, info)
        case "File" => Console.println("F " + List(name, date, version, info).mkString(" ")); File(name, date, version, info)
        case "Language" => { Console.println("L " + List(name, date, version, info).mkString(" ")); Language(name, date, version, info) }
      }
    }

  lazy val outputstat: Parser[Statistic] =
    "Output written on " ~> "\\w+\\.\\w+".r ~ ("(" ~> "\\d+".r <~ "pages, ") ~ "\\d+".r <~ "bytes)." ^^ {
      case output ~ pages ~ bytes =>
        PdfStatistic("pages", pages.toInt)
    }

  val encoding: Parser[String] = "defining Unicode char U\\+[0-9A-F]{4} \\(decimal \\d{1,5}\\)".r
  val stuffNoParen: Parser[String] = encoding | "[^\n()]+".r ^^ { case truie => Console.println("stuffNoParen : " + truie); truie }
  val stuff: Parser[String] = "[^\n]+".r ^^ { case truie => Console.println("stuff : " + truie); truie }
}