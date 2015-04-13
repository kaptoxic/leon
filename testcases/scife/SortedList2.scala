import scala.collection.immutable.Set
import leon.annotation._
import leon.lang._

object SortedList {
  sealed abstract class List
  case class Cons(head: Int, tail: List) extends List
  case object Nil extends List

  // proved with unrolling=0
  def size(l: List) : Int = (l match {
      case Nil => 0
      case Cons(_, t) => 1 + size(t)
  }) ensuring(res => res >= 0)

  def content(l: List): Set[Int] = l match {
    case Nil => Set()
    case Cons(i, t) => Set(i) ++ content(t)
  }

  def isSorted(l: List): Boolean = l match {
    case Nil => true
    case Cons(x, Nil) => true
    case Cons(x, Cons(y, ys)) => x <= y && isSorted(Cons(y, ys))
//    case Cons(x, ys) =>
//      content(ys).forall( y => x <= y )
  }
}
