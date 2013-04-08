package org.mtkachev.nlangp.pa2

/**
 * User: mick
 * Date: 08.04.13
 * Time: 17:33
 */
object Part2 extends App {
  val inTemp = new TaggedInput(args(0))
  val parametersTemp = new Parameters(inTemp.trees)
  val rareWords = parametersTemp.wordsCounts.filter(p => p._2 < 5)

  val trees = inTemp.trees.map(_.map{_ match {
    case n: Terminal => {
      if (rareWords.contains(n.word))
        Terminal(n.tag, "_RARE_")
      else n
    }
    case n => n
  }})


  val parameters = new Parameters(trees)
}
