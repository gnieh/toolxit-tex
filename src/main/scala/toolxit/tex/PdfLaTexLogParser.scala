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
   *  - Header
   *  - List of files loaded
   *  - Body (starts when "[1" encoutered
   *  - Output stats
   */
  lazy val logFile: Parser[PdfLaTeXLog] =
    header ~ opt(source) ~ rep(file | stuffNoBody ^^^ null) ~ body ~ rep(outputstat | stuff ^^^ null) ^^ {
      case header ~ fileList ~ body ~ statList => {
        PdfLaTeXLog(fileList.filter(_ != null),
          statList.filter(_ != null))
      }
    }

  lazy val header: Parser[Unit] =
    ("This is" ~> "\\w+".r <~ ",") ~ ("Version" ~> "[^\\s]+".r) ~ ("(" ~> "[^)]+".r <~ ")") ~ rep("[^*\n]+".r) ~ ("**" ~> "[^\n]+".r) ^^ {
      case compilo ~ version ~ distrib ~ options ~ source =>
        Console.println("Header: " + List(compilo, version, distrib, options, source).mkString("\n\t"))
    }

  lazy val source: Parser[Unit] =
    "(" ~> "[\\w-:/\\ .]+".r ^^ { case source => Console.println("Source: " + source) }

  // TODO: This seriously can't be considered as a tag for the "body", but it is the best I could find looking at the log...
  lazy val body: Parser[String] = "[1"

  lazy val file: Parser[TexCatalogFile] =
    /**
     * Accept following formats:
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
    ("(" ~> "[\\w-:/\\ .]+".r) ~ rep(description | file | packageMessage | stuffNoParen ^^^ null) <~ rep(stuffNoParen) <~ ")" ^^ {
      case path ~ catalogBody => {
        val grouped = catalogBody.filter(_ != null).groupBy {
          case _: TexCatalog => "catalog"
          case _: TexCatalogFile => "files"
          case _: PackageMessage => "messages"
        }
        Console.println("TC " + path);
        TexCatalogFile(path, grouped.getOrElse("catalog", Nil).map(_.asInstanceOf[TexCatalog]),
          grouped.getOrElse("files", Nil).map(_.asInstanceOf[TexCatalogFile]))
      }
    }

  lazy val description: Parser[TexCatalog] =
    /**
     * Accept following format:
     * Document Class: report 2007/10/19 v1.4h Standard LaTeX document class
     */
    ("[a-zA-Z ]+".r <~ ":") ~ "[\\w.-]+".r ~ "\\d{4}/\\d{2}/\\d{2}".r ~ "[\\w-]*v[^\\s]+".r ~ opt("[^\n]+".r) ^^ {
      case catalogType ~ name ~ date ~ version ~ verbose => catalogType match {
        case "Package" => Console.println("P " + List(name, date, version, verbose).mkString(" ")); Package(name, date, version, verbose)
        case "Document Class" => Console.println("C " + List(name, date, version, verbose).mkString(" ")); Class(name, date, version, verbose)
        case "File" => Console.println("F " + List(name, date, version, verbose).mkString(" ")); File(name, date, version, verbose)
        case "Language" => { Console.println("L " + List(name, date, version, verbose).mkString(" ")); Language(name, date, version, verbose) }
      }
    }

  lazy val packageMessage: Parser[PackageMessage] =
    /**
     * Accept following formats:
     * 
     * \li Package hyperref Info: Hyper figures OFF on input line 5757.
     * 
     * \li  Package etexcmds Info: Could not find \expanded.
     *      (etexcmds)             That can mean that you are not using pdfTeX 1.50 or
     *      (etexcmds)             that some package has redefined \expanded.
     *      (etexcmds)             In the latter case, load this package earlier.
     *      
     * \li Package Fancyhdr Warning: \headheight is too small (12.0pt): 
     *      Make it at least 14.49998pt.
     *      We now make it that large for the rest of the document.
     *      This may cause the page layout to be inconsistent, however.
     */
    "Package" ~> "[\\w.-]+".r ~ ("\\w+".r <~ ":") ~ "[^\n]+".r ~ rep((("(" ~> "[\\w.-]+".r ~> ")") | "\\s+".r) ~> "[^\n]+".r) ^^ {
      case name ~ messageType ~ info ~ infoNext =>
        {
          val message = info + infoNext.mkString(" ")
          Console.println("PM (" + messageType + ") " + name + ":" + message)
          messageType match {
            case "Info" => PackageInfo(name, message)
            case _ => UnkownPackageMessage(name, message)
          }
        }
    }

  lazy val outputstat: Parser[Statistic] =
    "Output written on " ~> "\\w+\\.\\w+".r ~ ("(" ~> "\\d+".r <~ "pages, ") ~ "\\d+".r <~ "bytes)." ^^ {
      case output ~ pages ~ bytes =>
        PdfStatistic("pages", pages.toInt)
    }

  // Some parsers to skip what we are not interested in
  val stuffNoBody: Parser[String] = "[^\\[\n]+".r ^^ { case truie => Console.println("stuffNB : " + truie); truie }
  val encoding: Parser[String] = "defining Unicode char U\\+[0-9A-F]{4} \\(decimal \\d{1,5}\\)".r
  val stuffNoParen: Parser[String] = encoding | "[^\n()]+".r ^^ { case truie => Console.println("stuffNoParen : " + truie); truie }
  val stuff: Parser[String] = "[^\n]+".r ^^ { case truie => Console.println("stuff : " + truie); truie }
}