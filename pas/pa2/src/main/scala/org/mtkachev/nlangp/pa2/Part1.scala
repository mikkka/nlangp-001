package org.mtkachev.nlangp.pa2

/**
 * User: mick
 * Date: 03.04.13
 * Time: 22:58
 */
object Part1 extends App {
  val parser = new TreeParser
  val trees = scala.io.Source.fromFile(args(0)).getLines().map(parser.toTree).toList

  val allTerminals = trees.flatMap(_.toList().collect{case n: Terminal => n})
  val allNonTerminals = trees.flatMap(_.toList().collect{case n: NonTerminal => n})

  val wordsCounts = allTerminals.map(_.word).groupBy(w => w).map(p => ((p._1), (p._2).size))
  val rareWords = wordsCounts.filter(p => p._2 < 5)

  val treesWithRare = trees.map(_.map{_ match {
    case n: Terminal => {
      if (rareWords.contains(n.word))
        Terminal(n.tag, "_RARE_")
      else n
    }
    case n => n
  }})

  treesWithRare.foreach(println)
}
