import leon.Utils._

object ExamplesAsSpecification {

  sealed abstract class List
  case class Cons(head: Int, tail: List) extends List
  case class Nil() extends List


  def identity(in: Int, inc: Int) = choose {
    (out: Int) => passes(Map((-1, 1) -> 0, (0, 1) -> 1, (41, 1) -> 42), (in, inc), out)
  }

}
