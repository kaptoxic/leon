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
  
  def bt1(v: Int) = Node(Black(), Empty(), v, Empty())
  val e = Empty()

  val ioExamples = List(
      Node(Black(),
        Node(Red(),
          bt1(1),
        2, e),
      3, e) ->
      Node(Red(),
        Node(Black(), e, 1, e),
      2, Node(Black(), e, 3, e))
  )
  
  
  def isBlack(t: Tree) : Boolean = t match {
    case Empty() => true
    case Node(Black(),_,_,_) => true
    case _ => false
  }

//  def redNodesHaveBlackChildren(t: Tree) : Boolean = t match {
//    case Empty() => true
//    case Node(Black(), l, _, r) => redNodesHaveBlackChildren(l) && redNodesHaveBlackChildren(r)
//    case Node(Red(), l, _, r) => isBlack(l) && isBlack(r) && redNodesHaveBlackChildren(l) && redNodesHaveBlackChildren(r)
//  }
//
//  def redDescHaveBlackChildren(t: Tree) : Boolean = t match {
//    case Empty() => true
//    case Node(_,l,_,r) => redNodesHaveBlackChildren(l) && redNodesHaveBlackChildren(r)
//  }
//
//  def blackBalanced(t : Tree) : Boolean = t match {
//    case Node(_,l,_,r) => blackBalanced(l) && blackBalanced(r) && blackHeight(l) == blackHeight(r)
//    case Empty() => true
//  }
//
//  def blackHeight(t : Tree) : Int = t match {
//    case Empty() => 1
//    case Node(Black(), l, _, _) => blackHeight(l) + 1
//    case Node(Red(), l, _, _) => blackHeight(l)
//  }
//  
//  def inv(t: Tree) = blackBalanced(t) && redDescHaveBlackChildren(t)
  
  def balance(c: Color, a: Tree, x: Int, b: Tree): Tree = (Node(c,a,x,b) match {
    case Node(Black(),Node(Red(),Node(Red(),a,xV,b),yV,c),zV,d) => 
      Node(Red(),Node(Black(),a,xV,b),yV,Node(Black(),c,zV,d))
    case Node(Black(),Node(Red(),a,xV,Node(Red(),b,yV,c)),zV,d) => 
      Node(Red(),Node(Black(),a,xV,b),yV,Node(Black(),c,zV,d))
    case Node(Black(),a,xV,Node(Red(),Node(Red(),b,yV,c),zV,d)) => 
      Node(Red(),Node(Black(),a,xV,b),yV,Node(Black(),c,zV,d))
    case Node(Black(),a,xV,Node(Red(),b,yV,Node(Red(),c,zV,d))) => 
      Node(Red(),Node(Black(),a,xV,b),yV,Node(Black(),c,zV,d))
    case Node(c,a,xV,b) => Node(c,a,xV,b)
  }) ensuring (res => content(res) == content(Node(c,a,x,b)))
  
  def getPartition(t: Node) = (t match {
    case Node(Black(),Node(Red(),Node(Red(),a,xV,b),yV,c),zV,d) => 
      1
    case Node(Black(),Node(Red(),a,xV,Node(Red(),b,yV,c)),zV,d) => 
      2
    case Node(Black(),a,xV,Node(Red(),Node(Red(),b,yV,c),zV,d)) => 
      3
    case Node(Black(),a,xV,Node(Red(),b,yV,Node(Red(),c,zV,d))) => 
      4
    case Node(c,a,xV,b) => 5
  })

  def main(args: Array[String]): Unit = {
    
    val a :: b :: c :: d :: Nil =
      (for (i <- 1 to 4) yield bt1(i)).toList
    val xV = 1
    val yV = 2
    val zV = 3
    
    val examples = List(
      Node(Black(),Node(Red(),Node(Red(),a,xV,b),yV,c),zV,d),
      Node(Black(),Node(Red(),a,xV,Node(Red(),b,yV,c)),zV,d),
      Node(Black(),a,xV,Node(Red(),Node(Red(),b,yV,c),zV,d)),
      Node(Black(),a,xV,Node(Red(),b,yV,Node(Red(),c,zV,d))),
      Node(Black(),a,xV,b),
      Node(Red(),a,xV,b)
    )    
    
    for ( (e, i) <- examples.init.zipWithIndex)
      assert( getPartition(e) == i + 1, "for " + e + " got index " + getPartition(e) + " and should be " + (i + 1) )     
    
    println( (
    for (e <- examples) yield {
      e.toString + " -> " + balance(e.color, e.left, e.value, e.right).toString
    } ).mkString(",\n") )
    

//    for ( (t@Node(c, l, v, r), o) <- ioExamples ) {
//      assert ( !inv(t) )
////      assert ( !inv(l) )
////      assert ( !inv(r) )
//      assert ( inv(balance(c, l, v, r)) ) 
//    }
    
  }
}
