/**
 * User: mick
 * Date: 09.05.13
 * Time: 15:34
 */
object Scorer {
  def vg(v: Vector[Double], features: Set[LocalFeatureSet],
         tag_2: String, tag_1: String, sentence: Array[String], i: Int, t: String): Double = {
    features.foldLeft(0.0) {(sum, lfs) =>
      sum + lfs.g(tag_2, tag_1, sentence, i, t).foldLeft(0.0)(_ + v(_))
    }
  }

  //feature idx -> number of encounter
  def f(taggedSentence: Array[WordTag], features: Set[LocalFeatureSet]): Map[Int,Int]  = {
    features.foldLeft(Map.empty[Int, Int]) {(acc, lfs) =>
      acc ++ lfs.g(taggedSentence)
    }
  }
}
