import collection.mutable
import java.io.PrintWriter

/**
 * User: mick
 * Date: 28.04.13
 * Time: 22:01
 */
class Params2 {
  private val jilmVal = mutable.Map.empty[(Int, Int, Int, Int), Double]


  def q(j: Int, i: Int, l: Int, m: Int) = jilmVal(j, i, l, m)

  def set(j: Int, i: Int, l: Int, m: Int, q: Double) {
    val key = (j, i, l, m)
    if (jilmVal.contains((key)))
      jilmVal.update(key, q)
    else
      throw new IllegalStateException("no key " + key)
  }

  def put(j: Int, i: Int, l: Int, m: Int, q: Double) {
    val key = (j, i, l, m)
    jilmVal.put(key, q)
  }
}

object Params2 {

  //train params 1 from corpus
  def apply(corpus: Corpus, params1: Params1): Params2 = {
    val params2 = new Params2
    //init
    for (
      k <- 0 to corpus.length - 1;
      (es, fs) = corpus.zipped(k);
      i <- 0 to fs.length - 1;
      j <- 0 to es.length - 1
    ) {
      val l = es.length
      val m = fs.length
      // e sentence contains _NULL_ so no need l + 1
      params2.put(j, i, l, m, 1.0 / l)
    }

    for (i <- 1 to 5) {
      println("train " + i)
      trainStep(params2, corpus, params1)
    }

    params2
  }

  def trainStep(params2: Params2, corpus: Corpus, params1: Params1) {
    val cEF = mutable.Map.empty[String, mutable.Map[String, Double]]
    val cE = mutable.Map.empty[String, Double]
    val cJILM = mutable.Map.empty[(Int, Int, Int, Int), Double]
    val cILM = mutable.Map.empty[(Int, Int, Int), Double]

    def cef(e: String, f: String) = cEF.getOrElseUpdate(e, mutable.Map.empty[String, Double]).getOrElse(f, 0.0)
    def cefset(e: String, f: String, v: Double) =
      cEF.getOrElseUpdate(e, mutable.Map.empty[String, Double]).put(f, v)

    def ce(e: String) = cE.getOrElseUpdate(e, 0.0)
    def ceset(e: String, v: Double) = cE.put(e, v)

    def teta(i: Int, j: Int, f: String, e: String, fs: Vector[String], es: Vector[String]): Double = {
      val l = es.length
      val m = fs.length
      (params2.q(j, i, l, m) * params1.t(f, e)) /
        (es.zipWithIndex.map(ei => params1.t(f, ei._1) * params2.q(ei._2, i, l, m)).sum)
    }

    for (
      k <- 0 to corpus.length - 1;
      (es, fs) = corpus.zipped(k);
      i <- 0 to fs.length - 1;
      j <- 0 to es.length - 1;
      e = es(j);
      f = fs(i)
    ) {
      val l = es.length
      val m = fs.length
      val t = teta(i, j, f, e, fs, es)

      cJILM.put((j,i, l, m), cJILM.getOrElse((j,i, l, m), 0.0) + t)
      cILM.put((i, l, m), cILM.getOrElse((i, l, m), 0.0) + t)

      cefset(e, f, cef(e, f) + t)
      ceset(e, ce(e) + t)
    }

    cEF.foreach(_ match {
      case (e, fMap) => {
        fMap.foreach(fVal => params1.set(fVal._1, e, fVal._2 / ce(e)))
      }
    })

    cJILM.foreach(_ match {
      case((j, i, l, m), value)  => {
        params2.set(j, i, l, m, cJILM(j, i, l, m) / cILM(i, l, m))
      }
    })
  }

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
