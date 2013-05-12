/**
 * User: mick
 * Date: 12.05.13
 * Time: 14:07
 */
object Part1 extends App {
  val params = FeaturesIO.readFeatures(args(0))
  val sentences = SentenceIO.readSentences(args(1))

  println(Algs.viterbi(params._1, params._2, sentences(1)))
}
