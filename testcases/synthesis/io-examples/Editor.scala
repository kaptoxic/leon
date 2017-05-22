import leon.lang._
import leon.lang.synthesis._

object EditorCut {
  sealed abstract class List
  case class Cons(head: Int, tail: List) extends List
  case object Nil extends List

//  val list1in = Cons(' ', Cons('a', Cons('b', Nil)))
//  val list1on = Cons('a', Cons('b', Nil))

  def cut(l: List) = choose {
    (out: List) => (l, out) passes {
      case Cons(0, Cons(1, Cons(2, Nil))) => Cons(1, Cons(2, Nil))
      case Cons(0, Cons(2, Nil)) => Cons(2, Nil)
    }
  }

}