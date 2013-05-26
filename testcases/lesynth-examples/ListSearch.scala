import leon.Utils._

object ListOperations {
    sealed abstract class List
    case class Cons(head: Int, tail: List) extends List
    case class Nil() extends List

    sealed abstract class Option
    case class None() extends Option
    case class Some(value: Int) extends Option
  
    def content(l: List) : Set[Int] = l match {
      case Nil() => Set.empty
      case Cons(head, tail) => Set(head) ++ content(tail)
    }

    def size(l: List) : Int = l match {
      case Nil() => 0
      case Cons(head, tail) => 1 + size(tail)
    }
    
    // returns integer that represents position in the list, if found
//    def linearSearch(l:List, x:Int): Option = {l match {
//      case Nil() => None()
//      case Cons(y,l1) => {
//        if (y==x) Some(0)
//        else {
//    	  linearSearch(l1,x) match {
//			case None() => None()
//			case Some(v) => Some(1 + v)
//	      }
//        }
//      }
//    }} ensuring ( res => res match {
//      case None() => (!contains(l,x))
//      case Some(i) => (lookup(l,i) == x)
//    })
    
    // linearSearch synthesis problem    
    def linearSearch(l: List, x: Int): Option = choose {
	  (res : Option) => res match {
        case None() => (!contains(l,x))
        case Some(i) => (lookup(l,i) == x)
	  }
    }
    
    // returns list element at position i
    def lookup(l:List, i:Int) : Int = {
      require(0 <= i && i < size(l))
      l match {
      	case Cons(x, l1) => if (i==0) x else lookup(l1, i-1)
      }
    } ensuring (res => contains(l,res))
    
    // returns true if l contains c
    def contains(l: List, c: Int): Boolean = {
      l match {
		case Nil() => false
		case Cons(lHead, lTail) =>
		  if (lHead == c) true
		  else contains(lTail, c)	  
      	}
    }
}
