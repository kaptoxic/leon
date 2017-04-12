import leon.lang.{ Map => _, _ }
import leon.lang.synthesis._

object ExamplesAsSpecification {

  sealed abstract class List
  case class Cons(head: Int, tail: List) extends List
  case class Nil() extends List

  def identity(in: Int) = choose {
    (out: Int) => (in, out) passes Map(-1 -> -1, 0 -> 0, 42 -> 42)
  }

  def tail(in: List) = choose {
    (out: List) => (in, out) passes {
      case Cons(0, Nil())                   => Nil()
      case Cons(0, Cons(1, Nil()))          => Cons(1, Nil())
      case Cons(0, Cons(1, Cons(2, Nil()))) => Cons(1, Cons(2, Nil()))
    }
  }

  def nil(in : List) = choose {
    (out : List) => (in, out) passes {
      case Nil() => Nil() 
      case Cons(0, Nil()) => Nil()
    }
  }
  
  def nilWithMap(in: List) = choose {
    (out : List) => (in, out) passes Map (
      Nil() -> Nil(),
      Cons(0, Nil()) -> Nil()
    )
  }
  
  val nilVal = Nil()
  val consVal = Cons(0, Nil())
  
  def nilWithMapVar(in: List) = choose {
    (out : List) => (in, out) passes Map (
      nilVal -> nilVal,
      consVal -> nilVal
    )
  }


}
