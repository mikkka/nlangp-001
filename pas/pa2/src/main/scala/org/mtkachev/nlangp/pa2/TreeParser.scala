package org.mtkachev.nlangp.pa2

import scala.util.parsing.combinator._
/**
 * User: mick
 * Date: 03.04.13
 * Time: 19:13
 */
class TreeParser extends JavaTokenParsers {
  def node : Parser[Node] = terminal | nonTerminal

  def terminal : Parser[Terminal] = "["~value~","~value~"]" ^^
    {case "["~tag~","~word~"]" => Terminal(trim(tag), trim(word))}

  def nonTerminal : Parser[NonTerminal] = "["~value~","~node~","~node~"]" ^^
    {case "["~tag~","~lft~","~rgt~"]" => NonTerminal(trim(tag), lft, rgt) }

  def value : Parser[String] = stringLiteral

  def toTree(line: String) = parseAll(node, line).get

  def trim(s: String) = s.dropRight(1).drop(1)
}


object TreeParserClient extends App {
  val parser = new TreeParser
  scala.io.Source.fromFile(args(0)).getLines().map(parser.toTree).foreach(println)
}