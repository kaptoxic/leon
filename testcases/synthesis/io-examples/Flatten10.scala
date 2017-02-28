import leon.lang._
import leon.lang.synthesis._

object Flatten {
  case class Tuple(fst: Int, snd: Int, thd: Int, fth: Int, f5: Int, f6: Int, f7: Int, f8: Int, f9: Int, f10: Int)

  sealed abstract class List
  case class Cons(head: Tuple, tail: List) extends List
  case object Nil extends List
  
  sealed abstract class IntList
  case class IntCons(head: Int, tail: IntList) extends IntList
  case object IntNil extends IntList

  def flatten(l: List): IntList = choose {
    (out: IntList) => (l, out) passes {
      case Nil => IntNil
      case Cons(Tuple(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), Nil) =>
        IntCons(1, IntCons(2, IntCons(3, IntCons(4, IntCons(5,
          IntCons(6, IntCons(7, IntCons(8, IntCons(9, IntCons(10, IntNil))))))))))
      case Cons(Tuple(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
        Cons(Tuple(11, 12, 13, 14, 15, 16, 17, 18, 19, 20), Nil)) =>
        IntCons(1, IntCons(2, IntCons(3, IntCons(4, IntCons(5,
          IntCons(6, IntCons(7, IntCons(8, IntCons(9, IntCons(10,
            IntCons(11, IntCons(12, IntCons(13, IntCons(14, IntCons(15,
              IntCons(16, IntCons(17, IntCons(18, IntCons(19, IntCons(20, IntNil))))))))))))))))))))
    }
  }

}
