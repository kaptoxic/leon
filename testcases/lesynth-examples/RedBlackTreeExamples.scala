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

  def balance(in: Tree): Tree =
    choose { (res: Tree) =>
      passes(
        Map[Tree, Tree](Node(Black(),
          Node(Red(),
            Node(Red(), Empty(), 1, Empty()),
          2, Empty()),
        3, Empty()) ->
        Node(Red(),
            Node(Black(), Empty(), 1, Empty()),
          2, Node(Black(), Empty(), 3, Empty()))
        ),
        in, res
      )
    }

}
