import leon.Utils._

object ExamplesAsSpecification {

  sealed abstract class List
  case class Cons(head: Int, tail: List) extends List
  case class Nil() extends List


  def identity(in: Int) = choose {
    (out: Int) => passes(Map(-1 -> -1, 0 -> 0, 42 -> 42), in, out)
  }

  def tail(in: List) = choose {
    (out: List) =>
      passes(Map[List, List](
        Cons(0, Nil())                   -> Nil(),
        Cons(0, Cons(1, Nil()))          -> Cons(1, Nil()),
        Cons(0, Cons(1, Cons(2, Nil()))) -> Cons(1, Cons(2, Nil()))
      ), in, out)
  }

  def nil(in : List) = choose {
    (out : List) =>
      passes(Map[List,List](
        Nil() -> Nil(),
        Cons(0, Nil()) -> Nil()
      ), in, out)
  }
}
