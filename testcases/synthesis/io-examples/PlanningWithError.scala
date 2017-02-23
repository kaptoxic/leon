import leon.lang.{ Map => _, _ }
import leon.lang.synthesis._
import leon.annotation._
import leon.collection._

object PlanningWithError {
  
  sealed abstract class Peg
  case object Src extends Peg
  case object Aux extends Peg
  case object Dst extends Peg
  
  abstract class State
  case object Initial extends State
  case class Move(disks: Int, src: Peg, dst: Peg, state: State) extends State
  
  case class Hanoi(disks: Int, src: Peg, aux: Peg, dst: Peg, state: State)
  
  def dec(value: Int) = value - 1
  
  def isZero(value: Int) = value == 0

  abstract class Status
  case object Halt extends Status
  case class Wait(t: Int) extends Status
  case class Report(msg: String) extends Status
  case object Error extends Status
		  
  def solveProblem(hanoi: Hanoi): (State, List[Status])  = choose {
    (res: (State, List[Status])) =>
      (hanoi, res) passes {
        case Hanoi(0, Src, Aux, Dst, Initial) =>
          (Move(0, Src, Dst, Initial), List(Halt))
        case Hanoi(1, Src, Aux, Dst, Initial) =>
          (
            Move(0, Aux, Dst,
    	    	  Move(1, Src, Dst,
  			        Move(0, Src, Aux, Initial))),
    		    List(Wait(5), Halt)
  		    )
        case Hanoi(2, Src, Aux, Dst, Initial) => 
          (
            Move(0, Src, Dst, 
      				Move(1, Aux, Dst,
      				  Move(0, Aux, Src,
        					Move(2, Src, Dst, 
        					  Move(0, Dst, Aux,
          						Move(1, Src, Aux,
          						  Move(0, Src, Dst, Initial)))))))
  				  ,
  				  List(Report("repair"), Wait(10), Halt)
				  )
        case Hanoi(0, Src, Aux, Dst, Move(0, Src, Dst, Initial)) =>
          (Move(0, Src, Dst, Initial), List(Halt))
	  }
  }
  
  def computeStatus(moves: State, previous: List[Status]): List[Status] = 
    previous match {
      case Cons(Halt, Nil()) =>
        List(Wait(5), Halt)
      case Cons(Wait(5), Cons(Halt, Cons(Wait(5), Cons(Halt, Nil())))) =>
			  List(Report("repair"), Wait(10), Halt)
    }
  
  def checkError(state: State) = 
    state match {
      case Move(0, Src, Dst, Initial) => false
      case _ => true
    }
    
  def solve(hanoi: Hanoi): (State, List[Status]) = hanoi match {
    case Hanoi(_, _, _, _, state) if !checkError(state) =>
      (state, List(Error))
    case Hanoi(0, src, _, dst, state) =>
      (Move(0, src, dst, state), List(Halt))
    case Hanoi(d, src, aux, dst, state) =>
      val (mv1, st1) = solve(Hanoi(d-1, aux, src, dst, state))
      val (mv2, st2) = solve(Hanoi(d-1, src, dst, aux, Move(d, src, dst, mv1)))
      val status = computeStatus(mv2, st1 ++ st2)
      (mv2, status)
  }
  
//  def main(args: Array[String]): Unit = {
//
//    val inOutPairs = Map(
//      Hanoi(0, Src, Aux, Dst, Initial) ->
//        (Move(0, Src, Dst, Initial), List(Halt))
//      ,
//      Hanoi(1, Src, Aux, Dst, Initial) ->
//        (
//          Move(0, Aux, Dst,
//  	    	  Move(1, Src, Dst,
//			        Move(0, Src, Aux, Initial))),
//  		    List(Wait(5), Halt)
//		    )
//      ,
//      Hanoi(2, Src, Aux, Dst, Initial) -> 
//        (
//          Move(0, Src, Dst, 
//    				Move(1, Aux, Dst,
//    				  Move(0, Aux, Src,
//      					Move(2, Src, Dst, 
//      					  Move(0, Dst, Aux,
//        						Move(1, Src, Aux,
//        						  Move(0, Src, Dst, Initial)))))))
//				  ,
//				  List(Report("repair"), Wait(10), Halt)
//			  )
//		  ,
//      Hanoi(0, Src, Aux, Dst, Move(0, Src, Dst, Initial)) ->
//        (Move(0, Src, Dst, Initial), List(Halt))
//    )
//    
//    for ((k, v) <- inOutPairs) assert(solve(k) == v)
//    
//  }
}