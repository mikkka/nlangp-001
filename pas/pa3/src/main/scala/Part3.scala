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
  val alignsEsEn = argmaxAligns(corpusEsEn, params1EsEn, params2EsEn)

  val alignIntersection =
    for(
      (ens, ess) <- alignsEnEs.zip(alignsEsEn);
      en <- ens;
      es <- ess
      if (en._2 == es._3 && en._3 == es._2)
    ) yield (en)

  FileIO.withPrintWriter(args(6)){pw =>
    for (
      (i, from, to) <- alignIntersection
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
