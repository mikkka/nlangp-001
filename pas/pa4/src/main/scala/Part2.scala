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
  val suffix1features = new SuffixFeatures(1)
  val suffix2features = new SuffixFeatures(2)
  val suffix3features = new SuffixFeatures(3)
  //find all features in input

  ss.foreach{xs =>
    trigramFeatures.findFeatures(xs)
    tagFeatures.findFeatures(xs)
    suffix1features.findFeatures(xs)
    suffix2features.findFeatures(xs)
    suffix3features.findFeatures(xs)
  }

  suffix3features.shift(suffix2features.shift(suffix1features.shift(tagFeatures.shift(trigramFeatures.shift(0)))))

  val v = Algs.perceptron(ss, Set(trigramFeatures, tagFeatures))

  val sentences = SentenceIO.readSentences(args(1))
  SentenceIO.writeTagged(args(2), sentences.map(s => Algs.viterbi(v, Set(trigramFeatures, tagFeatures), s)))
}
