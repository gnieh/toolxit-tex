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
package renderer

import toolxit.tex.PdfLaTeXLog

/**
 * This renderer simply returns the representation of the PdfLateX log file,
 *
 * @author Lucas Satabin
 * @author Audric Schiltknecht
 *
 */
class SimplePdfLaTeXRenderer(log: PdfLaTeXLog) {

  def render(): String = {
    "% Generated by ToolXiT TeX\n\n" +
      "List of Tex Catalog\n" +
      log.catalog.mkString("\n") +
      "\n\n" + "-" * 80 +
      "\n\nList of statistic\n" + 
      {
        for (PdfStatistic(name, value, _, _) <- log.statistics)
          yield name + " -> " + value + "\n"
      }.mkString("\n")
  }
}