import collection.mutable
import java.io.PrintWriter

/**
 * User: mick
 * Date: 28.04.13
 * Time: 22:01
 */
class Params2 {
  private val jilmVal = mutable.Map.empty[(Int, Int, Int, Int), Double]


  def q(j: Int, i: Int, l: Int, m: Int) = jilmVal((j, i, l, m))
  private def set(j: Int, i: Int, l: Int, m: Int, q: Double) {
    val key = (j, i, l, m)
    if (jilmVal.contains((key)))
      jilmVal.update(key, q)
    else
      throw new IllegalStateException("no key " + key)
  }

  private def put(j: Int, i: Int, l: Int, m: Int, q: Double) {
    val key = (j, i, l, m)
    jilmVal.put(key, q)
  }
}

object Params2 {
  //train params 1 from corpus
  def apply(corpus: Corpus, params1: Params1): Params2 = ???

  def apply(file: String): Params2 = {
    val p = new Params2
    FileIO.linesFromFile(file).foreach{l: String =>
      val splitted = l.split(" ")
      p.put(splitted(0).toInt, splitted(1).toInt, splitted(2).toInt, splitted(3).toInt, splitted(4).toDouble)
    }
    p
  }

  // save to file
  def save(params: Params2, file: String) = {
    FileIO.withPrintWriter(file){pw: PrintWriter =>
      params.jilmVal.foreach{kv =>
        pw.println("" + kv._1._1 + " " + kv._1._2 + " " + kv._1._3 + " " + kv._1._4 + " " + kv._2)
      }
    }
  }
}
