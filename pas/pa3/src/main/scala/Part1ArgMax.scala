/**
 * User: mick
 * Date: 27.04.13
 * Time: 15:10
 */
object Part1ArgMax extends App {
  val params1 = Params1(args(0))
  val corpus = new Corpus(args(1), args(2))

  val aligns = (for (
    tEF <- corpus.zipped
  ) yield {
    val eSentence = tEF._1
    val fSentence = tEF._2

    fSentence.zipWithIndex.map{fWord =>
      (fWord, eSentence.zipWithIndex.maxBy(eWord => params1.t(fWord._1, eWord._1)))
    }
  })

  FileIO.withPrintWriter(args(3)){pw =>
    for (
      s <- aligns.zipWithIndex;
      a <- s._1;
      if (a._2._2 > 0);
      i = s._2 + 1
    ) {
      val from = a._2._2
      val to = a._1._2 + 1
      pw.println(s"$i $from $to")
    }
  }
}
