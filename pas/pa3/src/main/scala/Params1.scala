import collection.mutable
import java.io.PrintWriter

/**
 * User: mick
 * Date: 25.04.13
 * Time: 17:03
 */
class Params1 {
  //e -> f -> val
  private val efval = mutable.Map.empty[String, mutable.Map[String, Double]]

  def t(f: String, e: String): Double = ???
  private def put(f: String, e: String, v: Double) = {
    efval.getOrElseUpdate(e, mutable.Map.empty[String, Double]).put(f, v)
  }
}

object Params1 {
  //train params 1 from corpus
  def apply(corpus: Corpus): Params1 = {
    new Params1
  }

  //train params 1 from file
  def apply(file: String): Params1 = {
    val p = new Params1
    FileIO.linesFromFile(file).foreach{l: String =>
      val splitted = l.split(" ")
      p.put(splitted(0), splitted(1), splitted(2).toDouble)
    }
    p
  }

  // save to file
  def save(params: Params1, file: String) = {
    FileIO.withPrintWriter(file){pw: PrintWriter =>
      params.efval.foreach{e =>
        e._2.foreach{f =>
          pw.println(e + " " + f._1 + " " + f._2)
        }
      }
    }
  }
}
