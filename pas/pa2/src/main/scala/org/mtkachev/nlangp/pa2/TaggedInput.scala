package org.mtkachev.nlangp.pa2

/**
 * User: mick
 * Date: 08.04.13
 * Time: 17:25
 */
class TaggedInput(fileName: String) {
  val parser = new TreeParser

  lazy val trees = scala.io.Source.fromFile(fileName).getLines().map(parser.toTree).toList
}
