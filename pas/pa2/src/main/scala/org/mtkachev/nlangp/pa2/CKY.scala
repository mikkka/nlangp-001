package org.mtkachev.nlangp.pa2

import collection.mutable

/**
 * User: mick
 * Date: 08.04.13
 * Time: 19:13
 */
object CKY {
  def dtParse(sentence: Vector[String], params: Parameters): Node = {
    def word(i: Int) = sentence(i - 1)

    val pi = mutable.Map.empty[(Int, Int, String), (Double, Node)]
    //init level
    for (
      i <- 1 to sentence.length;
      tag <- params.allTerminalTags
    ) {
      pi.put((i, i, tag), (params.q(UnaryRule(tag, word(i))), Terminal(tag, word(i))))
    }

    //pi table computing
    for (
      l <- 1 to (sentence.length - 1);
      i <- 1 to (sentence.length - l);
      tag <- params.allNonTerminalTags
    ) {
      val j = i + l
      val newPi = (for (
        rule <- params.rulesForTag(tag);
        s <- i to (j - 1)
      ) yield {
        val pi1 = pi.getOrElse((i, s, rule.to1), (0.0, null))
        val pi2 = pi.getOrElse(((s + 1, j, rule.to2)), (0.0, null))
        if (pi1._1 == 0.0 || pi2._1 == 0.0) (0.0, null)
        else ((params.q(rule) * pi1._1 * pi2._1), NonTerminal(rule.from, pi1._2, pi2._2))
      }).maxBy(_._1)

      pi.put((i, j, tag), newPi)
    }

    pi(1, sentence.length, "SBARQ")._2
  }
}
