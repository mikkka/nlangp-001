package org.mtkachev.nlangp.pa2

/**
 * User: mick
 * Date: 03.04.13
 * Time: 21:45
 */
abstract class Node {
  val tag: String
}

case class Terminal(tag: String, word: String) extends Node {
  override def toString = s"""[$tag, $word]"""
}
case class NonTerminal(tag: String, lft: Node, rgt: Node) extends Node {
  override def toString = s"""[$tag, $lft, $rgt]"""
}
