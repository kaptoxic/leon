import leon.Utils._

object HanoiImprovedObject {
  
  sealed abstract class List
  case class Cons(head: Move, tail: List) extends List
  case object Nil extends List
  
  def concat(l1: List, l2: List) : List = l1 match {
    case Nil => l2
    case Cons(h, t) =>
      Cons(h, concat(t, l2))
  } 
    
  sealed abstract class Peg
  case object Src extends Peg
  case object Aux extends Peg
  case object Dst extends Peg
  
  case class Move(src: Peg, dst: Peg)
  
  case class Hanoi(disks: Int, src: Peg, aux: Peg, dst: Peg)
  
  def dec(value: Int) = value - 1
  
  def isZero(value: Int) = value == 0
  
  val ioExamples = List(
    (Hanoi(0, Src, Aux, Dst) -> Cons(Move(Src, Dst), Nil)),
    (Hanoi(1, Src, Aux, Dst) ->
      Cons(Move(Aux, Dst),
        Cons(Move(Src, Dst),
          Cons(Move(Src, Aux), Nil)))
    ),
    (Hanoi(2, Src, Aux, Dst) -> 
      Cons(Move(Src, Dst), 
      Cons(Move(Aux, Dst),
      Cons(Move(Aux, Src),
      Cons(Move(Src, Dst), 
      Cons(Move(Dst, Aux),
      Cons(Move(Src, Aux),
      Cons(Move(Src, Dst),
        Nil)))))))
    )
  )
		  
//  def solve(hanoi: Hanoi) = choose {
//    (res: List) =>
//      passes(
//  	    Map[Hanoi, List]( ioExamples: _* ),
//        hanoi, res
//  	  )
//  }
    
  def solve(hanoi: Hanoi): List = hanoi match {
    case Hanoi(0, src, _, dst) => Cons(Move(src, dst), Nil)
    case Hanoi(d, src, aux, dst) =>
      concat(
        solve(Hanoi(d-1, aux, src, dst)),
        Cons(Move(src, dst),
    	    solve(Hanoi(d-1, src, dst, aux)))
      )
  }
  
  def main(args: Array[String]): Unit = {

    for ( (i, o) <- ioExamples ) {
      assert ( solve(i) == o ) 
    }
    
  }
}