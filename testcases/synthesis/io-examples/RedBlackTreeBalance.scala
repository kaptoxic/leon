import leon.lang._
import leon.lang.synthesis._
import leon.annotation._
import leon.collection._

object RedBlackTree {
  sealed abstract class Color
  case object Red extends Color
  case object Black extends Color

  sealed abstract class Tree
  case class Empty() extends Tree
  case class Node(color: Color, left: Tree, value: Int, right: Tree) extends Tree

  def content(t: Tree): Set[Int] = t match {
    case Empty() => Set.empty
    case Node(_, l, v, r) => content(l) ++ Set(v) ++ content(r)
  }

//  def size(t: Tree): Int = t match {
//    case Empty() => 0
//    case Node(_, l, v, r) => size(l) + 1 + size(r)
//  }
  
  /* We consider leaves to be black by definition */
  def isBlack(t: Tree): Boolean = t match {
    case Empty() => true
    case Node(Black, _, _, _) => true
    case _ => false
  }

  def redNodesHaveBlackChildren(t: Tree): Boolean = t match {
    case Empty() => true
    case Node(Black, l, _, r) => redNodesHaveBlackChildren(l) && redNodesHaveBlackChildren(r)
    case Node(Red, l, _, r) => isBlack(l) && isBlack(r) && redNodesHaveBlackChildren(l) && redNodesHaveBlackChildren(r)
  }

  def redDescHaveBlackChildren(t: Tree): Boolean = t match {
    case Empty() => true
    case Node(_, l, _, r) => redNodesHaveBlackChildren(l) && redNodesHaveBlackChildren(r)
  }

  def blackBalanced(t: Tree): Boolean = t match {
    case Node(_, l, _, r) => blackBalanced(l) && blackBalanced(r) && blackHeight(l) == blackHeight(r)
    case Empty() => true
  }

  def blackHeight(t: Tree): Int = t match {
    case Empty() => 1
    case Node(Black, l, _, _) => blackHeight(l) + 1
    case Node(Red, l, _, _) => blackHeight(l)
  }
  
  case class SortedTriple(min: Option[Int], max: Option[Int], sorted: Boolean)

  def isSortedTriple(tree: Tree) : SortedTriple = (tree match {
    case Empty() => SortedTriple(None(), None(), true)
    case Node(_, l, v, r) =>
      (isSortedTriple(l), isSortedTriple(r)) match {
        case (SortedTriple(minl, maxl, sortedl), SortedTriple(minr, maxr, sortedr)) =>
          val sorted = sortedl && sortedr && (v > maxl.getOrElse(v-1)) && (v < minr.getOrElse(v+1))
          SortedTriple(minl.orElse(Some(v)), maxr.orElse(Some(v)), sorted)
      }
  }) ensuring { res => res match {
    case SortedTriple(Some(min), Some(max), res) => !res || (min <= max)
    case SortedTriple(None(), None(), res) => res
    case _ => false
  }}

  def isSorted(tree: Tree): Boolean = isSortedTriple(tree).sorted
  
  def isSkewed(t: Tree) = t match {
    case Node(Red, l, _, Node(Red, rl, _, rr)) =>
      blackBalanced(l) && blackBalanced(rl) && blackBalanced(rr) &&
      redNodesHaveBlackChildren(l) && redNodesHaveBlackChildren(rl) && redNodesHaveBlackChildren(rr)
    case Node(Red, Node(Red, ll, _, lr), _, r) =>
      blackBalanced(r) && blackBalanced(ll) && blackBalanced(lr) &&
      redNodesHaveBlackChildren(r) && redNodesHaveBlackChildren(ll) && redNodesHaveBlackChildren(lr)
    case _ => false
  }
  
  def isRootSkewed(t: Node) = t match {
    case Node(Black, l: Tree, _, r: Tree) =>
      (isSkewed(l) && blackBalanced(r) && redNodesHaveBlackChildren(r)) ||
      (isSkewed(r) && blackBalanced(l) && redNodesHaveBlackChildren(l))
    case _ => false
  }

  
  def balanceTreeInput(t: Node): Tree = {
    require(
//      redNodesHaveBlackChildren (t.left) && redNodesHaveBlackChildren (t.right) &&
//      blackBalanced (t.left) && blackBalanced (t.right) && isSorted(t) &&
//      redDescHaveBlackChildren (t)
      isRootSkewed(t) && isSorted(t)
    )
    choose {
      (out: Tree) =>
        content(out) == content(t) &&
        redNodesHaveBlackChildren (out) &&
        blackBalanced (out) && isSorted(out)
    }
  }

  def balance(c: Color, a: Tree, x: Int, b: Tree): Tree = choose {
//  def balance(in: Tree): Tree = choose {
    (out: Tree) =>
      content(out) == content(a) ++ content(b) + x &&
      redNodesHaveBlackChildren (out) &&
      blackBalanced (out)
//    (out: Tree) => (in, out) passes {
//      case Node(Black, Node(Red, Node(Red, a, xV, b), yV, c), zV, d) =>
//        Node(Red, Node(Black, a, xV, b), yV, Node(Black, c, zV, d))
//      case Node(Black, Node(Red, a, xV, Node(Red, b, yV, c)), zV, d) =>
//        Node(Red, Node(Black, a, xV, b), yV, Node(Black, c, zV, d))
//      case Node(Black, a, xV, Node(Red, Node(Red, b, yV, c), zV, d)) =>
//        Node(Red, Node(Black, a, xV, b), yV, Node(Black, c, zV, d))
//      case Node(Black, a, xV, Node(Red, b, yV, Node(Red, c, zV, d))) =>
//        Node(Red, Node(Black, a, xV, b), yV, Node(Black, c, zV, d))
//      case Node(c, a, xV, b) =>
//        Node(c, a, xV, b)
//    }
  }
//  {
//    Node(c, a, x, b) match {
//      case Node(Black(), Node(Red(), Node(Red(), a, xV, b), yV, c), zV, d) =>
//        Node(Red(), Node(Black(), a, xV, b), yV, Node(Black(), c, zV, d))
//      case Node(Black(), Node(Red(), a, xV, Node(Red(), b, yV, c)), zV, d) =>
//        Node(Red(), Node(Black(), a, xV, b), yV, Node(Black(), c, zV, d))
//      case Node(Black(), a, xV, Node(Red(), Node(Red(), b, yV, c), zV, d)) =>
//        Node(Red(), Node(Black(), a, xV, b), yV, Node(Black(), c, zV, d))
//      case Node(Black(), a, xV, Node(Red(), b, yV, Node(Red(), c, zV, d))) =>
//        Node(Red(), Node(Black(), a, xV, b), yV, Node(Black(), c, zV, d))
//      case Node(c, a, xV, b) => Node(c, a, xV, b)
//    }
//  } ensuring (res => content(res) == content(Node(c, a, x, b)))

}
