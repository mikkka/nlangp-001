/**
 * User: mick
 * Date: 29.04.13
 * Time: 17:01
 */
object Part3 extends App {
  val corpusEnEs = new Corpus(args(0), args(1))
  val params1EnEs = Params1(args(2))
  val params2EnEs = Params2(args(3))

  val corpusEsEn = new Corpus(args(1), args(0))
  val params1EsEn = Params1(args(4))
  val params2EsEn = Params2(args(5))

  val alignsEnEs = argmaxAligns(corpusEnEs, params1EnEs, params2EnEs)
  val alignsEsEn = argmaxAligns(corpusEsEn, params1EsEn, params2EsEn).map(es => es.map(t => (t._1, t._3, t._2)))

  val aligns = (
    for(
      (ens, ess) <- alignsEnEs.zip(alignsEsEn)
    ) yield {
      //aligns intersection init
      val alignMap = (for (
        en <- ens;
        es <- ess
        if (en._2 == es._2 && en._3 == es._3)
      ) yield (en)).foldLeft(Map.empty[Int, Map[Int, Int]]) {(acc, a) =>
        val subMap = acc.getOrElse(a._2, Map.empty[Int, Int]) + (a._3 -> a._1)
        acc + (a._2 -> subMap)
      }

      //fold left on alignments union with intersection as starting point
      ((ens ++ ess ++ ens ++ ess ++ ens ++ ess)).foldLeft(alignMap){(acc, a) =>
        if (
          acc.getOrElse(a._2 - 1 , Map.empty[Int, Int]).contains(a._3)
            || acc.getOrElse(a._2 + 1, Map.empty[Int, Int]).contains(a._3)
            || acc.getOrElse(a._2, Map.empty[Int, Int]).contains(a._3 - 1)
            || acc.getOrElse(a._2, Map.empty[Int, Int]).contains(a._3 + 1)
            || acc.getOrElse(a._2 + 1, Map.empty[Int, Int]).contains(a._3 + 1)
            || acc.getOrElse(a._2 + 1, Map.empty[Int, Int]).contains(a._3 - 1)
            || acc.getOrElse(a._2 - 1, Map.empty[Int, Int]).contains(a._3 + 1)
            || acc.getOrElse(a._2 - 1, Map.empty[Int, Int]).contains(a._3 - 1)
        ) {
          val subMap = acc.getOrElse(a._2, Map.empty[Int, Int]) + (a._3 -> a._1)
          acc + (a._2 -> subMap)
        } else acc
      }.flatMap(kv => kv._2.map(t => (t._2, kv._1, t._1)))
    })

  FileIO.withPrintWriter(args(6)){pw =>
    for (
      (i, from, to) <- aligns.flatten
    ) {
      pw.println(s"$i $from $to")
    }
  }

  def argmaxAligns(corpus: Corpus, params1: Params1, params2: Params2) = {
    val aligns = (for (
      tEF <- corpus.zipped
    ) yield {
      val eSentence = tEF._1
      val fSentence = tEF._2

      fSentence.zipWithIndex.map{fWord =>
        val l = eSentence.length
        val m = fSentence.length
        val i = fWord._2
        (fWord, eSentence.zipWithIndex.maxBy(eWord => params1.t(fWord._1, eWord._1) * params2.q(eWord._2, i, l, m)))
      }
    })

    for (
      s <- aligns.zipWithIndex
    ) yield {
      (for (
        a <- s._1;
        if (a._2._2 > 0);
        i = s._2 + 1
      ) yield {
        val from = a._2._2
        val to = a._1._2 + 1
        (i, from, to)
      })
    }
  }
}
