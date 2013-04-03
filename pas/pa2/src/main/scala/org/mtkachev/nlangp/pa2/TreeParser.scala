package org.mtkachev.nlangp.pa2

import scala.util.parsing.combinator._
/**
 * User: mick
 * Date: 03.04.13
 * Time: 19:13
 */
class TreeParser extends JavaTokenParsers {
  //def value : Parser[Any] = arr | stringLiteral
  //def arr   : Parser[Any] = "["~repsep(value, ",")~"]"

  def node : Parser[Node] = terminal | nonTerminal

  def terminal : Parser[Terminal] = "["~stringLiteral~","~stringLiteral~"]" ^^
    {case "["~tag~","~word~"]" => Terminal(tag, word)}

  def nonTerminal : Parser[NonTerminal] = "["~stringLiteral~","~node~","~node~"]" ^^
    {case "["~tag~","~lft~","~rgt~"]" => NonTerminal(tag, lft, rgt) }

  def toTree(line: String) = parseAll(node, line).get
}


object TreeParserClient extends App {
  val parser = new TreeParser
  scala.io.Source.fromFile(args(0)).getLines().map(parser.toTree).foreach(println)
}