import java.io.{File, PrintWriter}
import java.util.regex.Pattern

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object Main extends App {
  val ocrParser = new OCRParser
  val printer = new PrintWriter("output.tsv")

  def normalize(str: String): String = str.replaceAll("[ ]+", " ")

  def intoFile(gene: String, panels: List[String]): Unit = {

    @tailrec
    def intoFileIter(panels: List[String]): Unit = {
      if(panels.isEmpty) return
      printer.println(s"${panels.head}\t$gene")
      intoFileIter(panels.tail)
    }

    intoFileIter(panels)
  }

  new File("./input").listFiles().toList.foreach { file =>

    val text = normalize(ocrParser.parsePDF(file))
    val start = "disease ID"

    val x = text.indexOf(start)

    val temp = text.substring(x+start.length+1, text.length)

    val agkjeajkg = "AARS EPILEPSY\nNEUROPATHIES\nINTELLECTUAL DISABILITY\nMENDELIOME\nPRECONCEPTION SCREENING"

    val pattern = Pattern.compile("[A-Z][[A-Z][0-9]]+ [[[A-Z]+(/)?]+]( )?[[[[A-Z]+(/)?]+] \n]*( )?[0-9]+\\.[0-9]+")

    val matcher = pattern.matcher(temp)

    //TODO pour ignorer le garbage: regarder si le match suivant fitte alphabetiquement entre le match d'avant et le match d'apres. Si non,

    while(matcher.find()) {
      val geneGenepanel = matcher.group.replaceAll("(\n)?( )?[0-9]+\\.[0-9]+", "")

      val intermediate = geneGenepanel.split("\n")

      val first = intermediate(0).split(" ")

      val gene = first(0)

      val panel = first.tail.mkString(" ")

      val panels = (intermediate.tail :+ panel).foldLeft(List[String]()){ (acc, panel) =>
        try{

          println(s"$gene panel: $panel")

          if(panel.charAt(panel.length-1) == ' ') acc :+ panel.substring(0, panel.length-1)
          else acc :+ panel

        } catch {
          case _: Exception =>
            acc
        }

      }

      intoFile(gene, panels)
    }

  }

  printer.close()

}
