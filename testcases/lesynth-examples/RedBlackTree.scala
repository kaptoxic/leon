import scala.collection.immutable.Set
//import scala.collection.immutable.Multiset

import leon.Utils._

object RedBlackTree { 
  sealed abstract class Color
  case class Red() extends Color
  case class Black() extends Color
 
  sealed abstract class Tree
  case class Empty() extends Tree
  case class Node(color: Color, left: Tree, value: Int, right: Tree) extends Tree

  def content(t : Tree) : Set[Int] = t match {
    case Empty() => Set.empty
    case Node(_, l, v, r) => content(l) ++ Set(v) ++ content(r)
  }

  def size(t : Tree) : Int = t match {
    case Empty() => 0
    case Node(_, l, v, r) => size(l) + 1 + size(r)
  }
  
  def isBlack(t: Tree) : Boolean = t match {
    case Empty() => true
    case Node(Black(),_,_,_) => true
    case _ => false
  }
  
//  def and(b1: Boolean, b2: Boolean) = b1 && b2

  def balance(c: Color, 
              c2: Color,
              c3: Color, a3: Tree, x3: Int, b3: Tree,
              a: Tree, x: Int, b: Tree,
              a2: Tree, x2: Int, b2: Tree): Tree = (
    choose { (res: Tree) => content(res) == content(Node(c,a,x,b)) }
  )

}
