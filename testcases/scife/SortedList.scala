import scala.collection.immutable.Set

import leon.lang.synthesis._

object SortedList {
  sealed abstract class List
  case class Cons(head: Int, tail: List) extends List
  case object Nil extends List

  def isSorted(l: List): Boolean = l match {
    case Nil => true
    case Cons(x, Nil) => true
    case Cons(x, Cons(y, ys)) => x <= y && isSorted(Cons(y, ys))
//    case Cons(x, ys) =>
//      content(ys).forall( y => x <= y )
  }
  
  def invariant(l: List) = isSorted(l)
  
  def enumerator = choose{ (res: List) => true }
  
  def size(l: List): Int = l match {
    case Nil => 0
    case Cons(x, xs) => 1 + size(xs)
  }

}