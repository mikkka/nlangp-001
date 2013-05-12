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
  val featureSet: Set[LocalFeatureSet] =
    Set(trigramFeatures, tagFeatures, suffix1features, suffix2features, suffix3features)

  //find all features in input
  ss.foreach{xs =>
    featureSet.foreach(_.findFeatures(xs))
  }
  featureSet.foldLeft(0)((acc, f) => f.shift(acc))

  val v = Algs.perceptron(ss, featureSet)

  val sentences = SentenceIO.readSentences(args(1))
  SentenceIO.writeTagged(args(2), sentences.map(s => Algs.viterbi(v, featureSet, s)))
}
