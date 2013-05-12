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
    val pi = scala.collection.mutable.Map((0, headTag, headTag) -> (0.0, headTag))
    val n = xs.length - 1

    def S(k: Int) =
      if (k < 0) List(headTag)
      else tags

    for (k <- 0 to n) {
      val xk = xs(k)
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
      u <- S(n - 2);
      s <- S(n - 1)
    ) yield {
      ((pi(n-1, u, s)._1 + Scorer.vg(v, features, u, s, xs, n, tailTag)) -> (u, s))
    }).maxBy(_._1)._2
    val retval = new Array[String](n)
    retval(n-1) = tagsTail._2
    retval(n-2) = tagsTail._1

    for(k <- (n - 3) to 0 by -1) {
      val i = k
      retval(i) = pi(k + 2, retval(i + 1), retval(i + 2))._2
    }

    xs.zipWithIndex.map(p => WordTag(p._1, retval(p._2)))
  }
}
