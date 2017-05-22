import leon.lang._
import leon.lang.synthesis._

object FlattenCombined {
  case class Pair(fst: Int, snd: Int)

  sealed abstract class List
  case class Cons(head: Pair, tail: List) extends List
  case object Nil extends List
  
  sealed abstract class IntList
  case class IntCons(head: Int, tail: IntList) extends IntList
  case object IntNil extends IntList
  
  def contentsP(l: List): Set[Int] = l match {
    case Nil => Set.empty
    case Cons(Pair(fst, snd), xs) => contentsP(xs) ++ Set(fst, snd)
  }

  def contents(l: IntList): Set[Int] = l match {
    case IntNil => Set.empty
    case IntCons(x, xs) => contents(xs) ++ Set(x)
  }

  def flatten(l: List): IntList = choose {
    (out: IntList) =>
      (contents(out) == contentsP(l)) && ((l, out) passes {
        case Cons(Pair(1, 2), Nil) => IntCons(1, IntCons(2, IntNil))
      })
  }
  
}
