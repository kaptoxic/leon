import leon.Utils._

object ListOperations {
    sealed abstract class List
    case class Cons(head: Int, tail: List) extends List
    case class Nil() extends List

    def content(l: List) : Set[Int] = l match {
      case Nil() => Set.empty
      case Cons(head, tail) => Set(head) ++ content(tail)
    }

    def size(l: List) : Int = l match {
      case Nil() => 0
      case Cons(head, tail) => 1 + size(tail)
    }
    
    // returns integer that represents position in the list, if found
    def linearSearch(l:List, x:Int): Int = {l match {
      case Nil() => -1
      case Cons(y,l1) => {
        if (y==x) 0 else {
    	  linearSearch(l1, x) match {
			case v if v == -1 => -1
			case v => 1 + v
	      }
        }
      }
    }} ensuring ( (res: Int) => res match {
      case i if i < 0 => (!contains(l,x))
      case i => (lookup(l,i) == x)
    })
    
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
