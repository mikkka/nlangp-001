package org.mtkachev.nlangp.pa2

/**
 * User: mick
 * Date: 08.04.13
 * Time: 17:29
 */
class Parameters(trees: List[Node]) {
  lazy val flatTrees = trees.flatMap(_.toList())

  lazy val allUnary = flatTrees.collect{case n: Terminal => n}
  lazy val allBinary = flatTrees.collect{case n: NonTerminal => n}
  lazy val nonTerminalCounts = flatTrees.map(_.tag).groupBy(w => w).map(p => ((p._1), (p._2).size))

  lazy val wordsCounts = allUnary.map(_.word).groupBy(w => w).map(p => ((p._1), (p._2).size))
  lazy val rareWords = wordsCounts.filter(p => p._2 < 5)
}


case class UnaryRule(from: String, to: String)
case class BinaryRule(from: String, to1: String , to2: String)