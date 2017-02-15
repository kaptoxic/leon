import leon.lang._
import leon.lang.synthesis._

object MergeSort {
  sealed abstract class List
  case class Cons(head: Int, tail: List) extends List
  case object Nil extends List

//  def contents(l: List): Set[Int] = l match {
//    case Nil => Set.empty
//    case Cons(x, xs) => contents(xs) ++ Set(x)
//  }
//
//  def isSorted(l: List): Boolean = l match {
//    case Nil => true
//    case Cons(x, xs) => xs match {
//      case Nil => true
//      case Cons(y, ys) => x <= y && isSorted(Cons(y, ys))
//    }
//  }
//  
//  def size(list: List): Int = list match {
//    case Nil => 0
//    case Cons(x, xs) => 1 + size(xs)
//  }

  def merge(l1: List, l2: List): List = choose {
    (out: List) => ((l1, l2), out) passes {
//      case (Cons(-1, Cons(1, Cons(3, Nil))), Cons(0, Cons(2, Nil))) =>
//        Cons(-1, Cons(0, Cons(1, Cons(2, Cons(3, Nil)))))
      case (Cons(1, Cons(3, Nil)), Cons(0, Cons(2, Nil))) =>
        Cons(0, Cons(1, Cons(2, Cons(3, Nil))))
      case (Cons(1, Cons(3, Nil)), Cons(2, Nil)) =>
        Cons(1, Cons(2, Cons(3, Nil)))
      case (Cons(3, Nil), Cons(2, Nil)) =>
        Cons(2, Cons(3, Nil))
      case (Cons(3, Nil), Nil) =>
        Cons(3, Nil)
      case (Nil, Nil) =>
        Nil
//      case (Cons(0, Nil), Cons(1, Nil)) =>
//        Cons(0, Cons(1, Nil))
//      case (Cons(0, Nil), Cons(2, Nil)) =>
//        Cons(0, Cons(2, Nil))
//      case (Cons(0, Cons(1, Nil)), Cons(2, Nil)) =>
//        Cons(0, Cons(1, Cons(2, Nil)))
//      case (Cons(0, Cons(2, Nil)), Cons(1, Nil)) =>
//        Cons(0, Cons(1, Cons(2, Nil)))
    }
  }

}
