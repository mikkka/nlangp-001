/**
 * User: mick
 * Date: 09.05.13
 * Time: 17:29
 */
import Aliases._
import Const._

object Algs {
  /**
   * @param v
   * @param features
   * @param xs sentence without heading * * and tail STOP
   * @return
   */
  def viterbi(v: V, features: Set[LocalFeatureSet], xs: Sentence): TaggedSentence = {
    val pi = scala.collection.mutable.Map((-1, headTag, headTag) -> (0.0, headTag))
    val n = xs.length - 1

    def S(k: Int) =
      if (k < 0) List(headTag)
      else tags

    for (k <- 0 to n) {
      for (
        u <- S(k-1);
        s <- S(k)
      ) {
        val pival = (for(t <- S(k - 2)) yield {
          val piw = pi(k-1,t,u)._1 + Scorer.vg(v, features, t, u, xs, k, s)
          (piw, t)
        }).maxBy(_._1)
        pi += ((k, u, s) -> pival)
      }
    }

    val tagsTail = (for(
      u <- S(n - 1);
      s <- S(n)
    ) yield {
      val score = Scorer.vg(v, features, u, s, xs, n + 1, tailTag)
      ((pi(n, u, s)._1 + score) -> (u, s))
    }).maxBy(_._1)._2

    val retval = new Array[String](n + 1)
    retval(n) = tagsTail._2
    retval(n-1) = tagsTail._1

    for(k <- (n - 2) to 0 by -1) {
      val i = k
      retval(i) = pi(k + 2, retval(i + 1), retval(i + 2))._2
    }

    xs.zipWithIndex.map(p => WordTag(p._1, retval(p._2)))
  }

  def perceptron(ss: Vector[TaggedSentence], features: Set[LocalFeatureSet]) {
    val size = features.foldLeft(0){(acc, f) => acc + f.size}
    var v = Array.fill[Double](size)(0).toVector

    for (i <- 1 to 5) {
      for (taggedSentence <- ss) {
        val goldTagging = taggedSentence.drop(1).dropRight(2)
        val xs = goldTagging.map(_.word)
        val bestTagging = viterbi(v, features, xs)

        if (bestTagging.toList != goldTagging.toList) {
          val goldTaggingV = Scorer.f(goldTagging, features)
          val bestTaggingV = Scorer.f(bestTagging, features)

          v = goldTaggingV.foldLeft(v){(res, t) =>
            val curVal = res(t._1)
            res.updated(t._1, curVal + t._2)
          }
          v = bestTaggingV.foldLeft(v){(res, t) =>
            val curVal = res(t._1)
            res.updated(t._1, curVal - t._2)
          }
        }
      }
    }
  }
}
