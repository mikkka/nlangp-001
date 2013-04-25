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

  def t(f: String, e: String): Double = efval(e)(f)
  def n(e: String): Int = efval(e).size
  def es = efval.keySet

  private def put(f: String, e: String, v: Double) = {
    efval.getOrElseUpdate(e, mutable.Map.empty[String, Double]).put(f, v)
  }

  private def set(e: String, v: Double): Unit = {
    val ef = efval(e)
    val keys = ef.keySet
    keys.foreach(f => this.set(f, e, v))
    Unit
  }

  private def set(f: String, e: String, v: Double): Unit = {
    efval(e).update(f, v)
    Unit
  }
}

object Params1 {
  //train params 1 from corpus
  def apply(corpus: Corpus): Params1 = {
    val params = new Params1
    //init step
    corpus.zipped.foreach(_ match {
      case (eSentences, fSentences) => {
        for (
          e <- eSentences;
          f <- fSentences
        ) {
          params.put(f, e, 0)
        }
      }
    })

    for (
      e <- params.es
    ) {
      val n = params.n(e)
      params.set(e, 1.0/n)
    }

    params
  }

  //train params 1 from file
  def apply(file: String): Params1 = {
    val p = new Params1
    FileIO.linesFromFile(file).foreach{l: String =>
      val splitted = l.split(" ")
      p.set(splitted(0), splitted(1), splitted(2).toDouble)
    }
    p
  }

  // save to file
  def save(params: Params1, file: String) = {
    FileIO.withPrintWriter(file){pw: PrintWriter =>
      params.efval.foreach{e =>
        e._2.foreach{f =>
          pw.println(e._1 + " " + f._1 + " " + f._2)
        }
      }
    }
  }
}
