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

import scala.util.parsing.input.Positional

// a raw entry as returned by the parser before it is refined for later use
sealed trait Raw extends Positional

final case class PdfLaTeXLog(catalog: List[TexCatalogFile],
                             statistics: List[Statistic])

final case class TexCatalogFile(path: String,
                                catalog: List[TexCatalog],
                                includes: List[TexCatalogFile]) extends Raw

sealed trait TexCatalog {
  val name: String
  val date: String
  val version: String
  val info: Option[String]

  override def toString: String = "Name: " + name + " - Date: " + date + " - Version: " + version + " - Info: " + info
}

final case class Class(name: String,
                       date: String,
                       version: String,
                       info: Option[String]) extends TexCatalog
final case class Package(name: String,
                         date: String,
                         version: String,
                         info: Option[String]) extends TexCatalog
final case class File(name: String,
                      date: String,
                      version: String,
                      info: Option[String]) extends TexCatalog
final case class Language(name: String,
                          date: String,
                          version: String,
                          info: Option[String]) extends TexCatalog

// Statistic related stuff
sealed trait Statistic extends Raw

final case class TeXStatistic(name: String,
                              value: Int,
                              max: Option[Int] = None) extends Statistic
final case class PdfStatistic(name: String,
                              value: Int,
                              pool: Option[Int] = None,
                              max: Option[Int] = None) extends Statistic