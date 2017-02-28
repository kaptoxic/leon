import leon.lang._
import leon.lang.synthesis._

object Flatten {
  case class Tuple(fst: Int, snd: Int, thd: Int, fth: Int)

  sealed abstract class List
  case class Cons(head: Tuple, tail: List) extends List
  case object Nil extends List
  
  sealed abstract class IntList
  case class IntCons(head: Int, tail: IntList) extends IntList
  case object IntNil extends IntList

  def flatten(l: List): IntList = choose {
    (out: IntList) => (l, out) passes {
      case Nil => IntNil
      case Cons(Tuple(1, 2, 5, 6), Nil) => IntCons(1, IntCons(2, IntCons(5, IntCons(6, IntNil))))
      case Cons(Tuple(2, 3, 5, 6), Cons(Tuple(1, 4, 7, 8), Nil)) =>
        IntCons(2, IntCons(3, IntCons(5, IntCons(6,
          IntCons(1, IntCons(4, IntCons(7, IntCons(8, IntNil))))))))
    }
  }

}
