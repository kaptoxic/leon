import leon.lang._
import leon.lang.synthesis._

object Flatten {
  case class Pair(fst: Int, snd: Int)

  sealed abstract class List
  case class Cons(head: Pair, tail: List) extends List
  case object Nil extends List
  
  sealed abstract class IntList
  case class IntCons(head: Int, tail: IntList) extends IntList
  case object IntNil extends IntList

  def flatten(l: List): IntList = choose {
    (out: IntList) => (l, out) passes {
      case Nil => IntNil
      case Cons(Pair(1, 2), Nil) => IntCons(1, IntCons(2, IntNil))
      case Cons(Pair(2, 3), Cons(Pair(1, 4), Nil)) => IntCons(2, IntCons(3, IntCons(1, IntCons(4, IntNil))))
    }
  }

}
