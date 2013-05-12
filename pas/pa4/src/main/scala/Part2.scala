import scala.Array

/**
 * User: mick
 * Date: 12.05.13
 * Time: 16:19
 */
object Part2 extends App {
  val ss = SentenceIO.readTaggedSentences(args(0))

  val trigramFeatures = new TrigramsFeatures
  val tagFeatures = new TagFeatures
  //find all features in input

  ss.foreach{xs =>
    trigramFeatures.findFeatures(xs)
    tagFeatures.findFeatures(xs)
  }

  trigramFeatures.shift(0)
  tagFeatures.shift(trigramFeatures.size)

  val v = Algs.perceptron(ss, Set(trigramFeatures, tagFeatures))

  val sentences = SentenceIO.readSentences(args(1))
  SentenceIO.writeTagged(args(2), sentences.map(s => Algs.viterbi(v, Set(trigramFeatures, tagFeatures), s)))
}
