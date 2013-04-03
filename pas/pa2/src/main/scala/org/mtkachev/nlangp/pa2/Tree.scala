package org.mtkachev.nlangp.pa2

/**
 * User: mick
 * Date: 03.04.13
 * Time: 21:45
 */
abstract class Node {
  val tag: String

  def filter(f: Node => Boolean): List[Node]
  def toList(): List[Node]
  def map(f: Node => Node): Node
}

case class Terminal(tag: String, word: String) extends Node {
  def filter(f: (Node) => Boolean): List[Node] =
    if (f(this)) List(this)
    else Nil

  def toList(): List[Node] = List(this)

  def map(f: (Node) => Node): Node =
    f(this)

  override def toString = s"""["$tag", "$word"]"""
}
case class NonTerminal(tag: String, lft: Node, rgt: Node) extends Node {
  def filter(f: (Node) => Boolean): List[Node] =
    if (f(this)) this :: (lft.filter(f) ::: rgt.filter(f))
    else lft.filter(f) ::: rgt.filter(f)

  def toList(): List[Node] = this :: (lft.toList() ::: rgt.toList())

  def map(f: (Node) => Node): Node =
    f(NonTerminal(tag, lft.map(f), rgt.map(f)))

  override def toString = s"""["$tag", $lft, $rgt]"""
}
