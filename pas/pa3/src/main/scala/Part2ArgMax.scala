/**
 * User: mick
 * Date: 27.04.13
 * Time: 15:10
 */
object Part2ArgMax extends App {
  val params1 = Params1(args(0))
  val params2 = Params2(args(1))
  val corpus = new Corpus(args(2), args(3))

  println("data read")

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

  println("alignments found")

  FileIO.withPrintWriter(args(4)){pw =>
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
