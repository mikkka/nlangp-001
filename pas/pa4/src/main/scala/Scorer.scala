/**
 * User: mick
 * Date: 09.05.13
 * Time: 15:34
 */
import Aliases._

object Scorer {
  def vg(v: V, features: Set[LocalFeatureSet],
         tag_2: Tag, tag_1: Tag, sentence: Sentence, i: Int, t: Tag): Double = {
    features.foldLeft(0.0) {(sum, lfs) =>
      sum + lfs.g(tag_2, tag_1, sentence, i, t).foldLeft(0.0)(_ + v(_))
    }
  }

  //feature idx -> number of encounter
  def f(taggedSentence: TaggedSentence, features: Set[LocalFeatureSet]): FVector = {
    features.foldLeft(Map.empty[Int, Int]) {(acc, lfs) =>
      acc ++ lfs.g(taggedSentence)
    }
  }
}
