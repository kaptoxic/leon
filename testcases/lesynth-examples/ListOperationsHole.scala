import scala.collection.immutable.Set

import leon.Utils._

object ListOperations {
    sealed abstract class List
    case class Cons(head: Int, tail: List) extends List
    case class Nil() extends List

	  def content(l: List) : Set[Int] = l match {
	    case Nil() => Set.empty
	    case Cons(head, tail) => Set(head) ++ content(tail)
	  }   
   
	  def size(l: List) : Int = (l match {
	      case Nil() => 0
	      case Cons(_, t) => 1 + size(t)
	  }) ensuring(res => res >= 0) 
    
    def concat(l1: List, l2: List) : List = choose {
    (out : List) =>
      content(out) == content(l1) ++ content(l2) &&
      size(out) == size(l1) + size(l2)
    }

}
