/**
 * User: mick
 * Date: 27.04.13
 * Time: 15:10
 */
object Part1ArgMax extends App {
  val params1 = Params1(args(0))
  val corpus = new Corpus(args(1), args(2))

  val aligns = (for (
    tEF <- corpus.zipped.zipWithIndex
  ) yield {
    val eSentence = tEF._1._1
    val fSentence = tEF._1._2
    val i = tEF._2

    fSentence.zipWithIndex.map{fWord =>
      (fWord, eSentence.zipWithIndex.maxBy(eWord => params1.t(fWord._1, eWord._1)))
    }
  })

  println(aligns)
}
