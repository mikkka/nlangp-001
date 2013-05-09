/**
 * User: mick
 * Date: 09.05.13
 * Time: 15:34
 */
object Scorer {
  def vg(v: Vector[Double], features: Set[LocalFeatureSet],
         tag_2: String, tag_1: String, sentence: Vector[String], i: Int, t: String): Double = {
    features.foldLeft(0.0) {(sum, lfs) =>
      sum + lfs.g(tag_2, tag_1, sentence, i, t).foldLeft(0.0)(_ + v(_))
    }
  }
}
