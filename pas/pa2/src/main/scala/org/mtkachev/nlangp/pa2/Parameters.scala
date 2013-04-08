package org.mtkachev.nlangp.pa2

/**
 * User: mick
 * Date: 08.04.13
 * Time: 17:29
 */
class Parameters(trees: List[Node]) {
  lazy val flatTrees = trees.flatMap(_.toList())

  lazy val allUnary = flatTrees.collect{case n: Terminal => n}.map(n => UnaryRule(n.tag, n.word))
  lazy val allBinary = flatTrees.collect{case n: NonTerminal => n}.map(n => BinaryRule(n.tag, n.lft.tag, n.rgt.tag))

  lazy val wordsCounts = allUnary.map(_.to).groupBy(w => w).map(p => ((p._1), (p._2).size))
  lazy val rareWords = wordsCounts.filter(p => p._2 < 5)

  lazy val nonTerminalCounts = flatTrees.map(_.tag).groupBy(w => w).map(p => ((p._1), (p._2).size))
  lazy val unaryCounts = allUnary.groupBy(w => w).map(p => ((p._1), (p._2).size))
  lazy val binaryCounts = allBinary.groupBy(w => w).map(p => ((p._1), (p._2).size))

  lazy val allTerminalTags = unaryCounts.keySet.map(_.from).toSet
  lazy val allNonTerminalTags = binaryCounts.keySet.map(_.from).toSet

  lazy val rulesForTag = binaryCounts.keySet.groupBy(_.from)

  def q(rule: UnaryRule) = {
    val trueRule = if (wordsCounts.contains(rule.to)) UnaryRule(rule.from, rule.to) else UnaryRule(rule.from, "_RARE_")
    1.0 * unaryCounts(trueRule) / nonTerminalCounts(trueRule.from)
  }
  def q(rule: BinaryRule) = 1.0 * binaryCounts.getOrElse(rule, 0) / nonTerminalCounts(rule.from)
}


case class UnaryRule(from: String, to: String)
case class BinaryRule(from: String, to1: String , to2: String)