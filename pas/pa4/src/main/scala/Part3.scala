
/**
 * User: mick
 * Date: 12.05.13
 * Time: 16:19
 */
object Part3 extends App {
  val ss = SentenceIO.readTaggedSentences(args(0))

  val trigramFeatures = new TrigramsFeatures
  val tagFeatures = new TagFeatures
  val suffix1features = new WordPartFeatures(new SuffixKeyGen(1), 1)
  val suffix2features = new WordPartFeatures(new SuffixKeyGen(2), 2)
  val suffix3features = new WordPartFeatures(new SuffixKeyGen(3), 3)

  val prefix1features = new WordPartFeatures(new PrefixKeyGen(1), 1)
  val prefix2features = new WordPartFeatures(new PrefixKeyGen(2), 2)
  val prefix3features = new WordPartFeatures(new PrefixKeyGen(3), 3)

  val featureSet: Set[LocalFeatureSet] =
    Set(trigramFeatures, tagFeatures,
      suffix1features, suffix2features, suffix3features,
      prefix1features, prefix2features, prefix3features
    )

  //find all features in input
  ss.foreach{xs =>
    featureSet.foreach(_.findFeatures(xs))
  }
  featureSet.foldLeft(0)((acc, f) => f.shift(acc))

  val v = Algs.perceptron(ss, featureSet)

  val sentences = SentenceIO.readSentences(args(1))
  SentenceIO.writeTagged(args(2), sentences.map(s => Algs.viterbi(v, featureSet, s)))
}
