package toolxit.tex
package test

import renderer._
import java.io.{ FileInputStream, InputStreamReader, FileOutputStream, OutputStreamWriter, File }
import scala.util.Properties

object LogParser extends App {

  import PdfLaTeXLogParser._

  val input = if (args.length == 0)
    "doc/pdflatex.out"
  else
    args(0)

  val encoding = if (args.length < 2)
    "ISO-8859-15"
  else
    args(1)
    
    
  Console.println("Parsing file: " + input.toString)

  parseAll(logFile,
    new InputStreamReader(new FileInputStream(input), encoding)) match {
      case Success(res, _) =>
        val logrenderer = new SimplePdfLaTeXRenderer(res)
        val file = new File(Properties.userHome, "test.log")

        file.createNewFile
        val logwriter = new OutputStreamWriter(
          new FileOutputStream(file), encoding)
        logwriter.write(logrenderer.render)
        logwriter.flush
        logwriter.close

      case fail => println(fail)
    }
}