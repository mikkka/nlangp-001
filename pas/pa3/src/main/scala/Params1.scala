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

  def set(f: String, e: String, v: Double): Unit = {
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

    for (i <- 1 to 5) {
      println("step " + i)
      trainStep(params, corpus)
    }
    println("train complete")

    params
  }

  def trainStep(params: Params1, corpus: Corpus) : Unit = {
    val cEF = mutable.Map.empty[String, mutable.Map[String, Double]]
    val cE = mutable.Map.empty[String, Double]

    def cef(e: String, f: String) = cEF.getOrElseUpdate(e, mutable.Map.empty[String, Double]).getOrElse(f, 0.0)
    def cefset(e: String, f: String, v: Double) =
      cEF.getOrElseUpdate(e, mutable.Map.empty[String, Double]).put(f, v)

    def ce(e: String) = cE.getOrElseUpdate(e, 0.0)
    def ceset(e: String, v: Double) = cE.put(e, v)

    def teta(f: String, e: String, es: Vector[String]): Double = params.t(f, e) / es.map(params.t(f, _)).sum

    for (
      k <- 0 to corpus.length - 1;
      (es, fs) = corpus.zipped(k);
      i <- 0 to fs.length - 1;
      j <- 0 to es.length - 1;
      e = es(j);
      f = fs(i)
    ) {
      val t = teta(f, e, es)
      cefset(e, f, cef(e, f) + t)
      ceset(e, ce(e) + t)
    }

    cEF.foreach(_ match {
      case (e, fMap) => {
        fMap.foreach(fVal => params.set(fVal._1, e, fVal._2 / ce(e)))
      }
    })

  }

  //train params 1 from file
  def apply(file: String): Params1 = {
    val p = new Params1
    FileIO.linesFromFile(file).foreach{l: String =>
      val splitted = l.split(" ")
      p.put(splitted(1), splitted(0), splitted(2).toDouble)
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
