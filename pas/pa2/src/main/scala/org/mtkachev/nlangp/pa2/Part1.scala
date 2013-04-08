package org.mtkachev.nlangp.pa2

import java.io.PrintWriter

/**
 * User: mick
 * Date: 03.04.13
 * Time: 22:58
 */
object Part1 extends App {
  val in = new TaggedInput(args(0))
  val parameters = new Parameters(in.trees)
  val rareWords = parameters.wordsCounts.filter(p => p._2 < 5)

  val treesWithRare = in.trees.map(_.map{_ match {
    case n: Terminal => {
      if (rareWords.contains(n.word))
        Terminal(n.tag, "_RARE_")
      else n
    }
    case n => n
  }})
  Some(new PrintWriter(args(1))).foreach{p =>
    treesWithRare.foreach(node => p.write(node.toString + "\n"))
    p.close
  }
}
