import leon.lang.{ Map => _, _ }
import leon.lang.synthesis._
import leon.annotation._
import leon.collection._

object HanoiObject {
  
  sealed abstract class Peg
  case object Src extends Peg
  case object Aux extends Peg
  case object Dst extends Peg
  
  abstract class State
  case object Initial extends State
  case class Move(disks: Int, src: Peg, dst: Peg) extends State
  
  case class Hanoi(disks: Int, src: Peg, aux: Peg, dst: Peg)
  
  def dec(value: Int) = value - 1
  
  def isZero(value: Int) = value == 0

  abstract class Status
  case object Halt extends Status
  case class Wait(t: Int) extends Status
  case class Report(msg: String) extends Status
		  
  def solveArray(hanoi: Hanoi): (List[Move], List[Status])  = choose {
    (res: (List[Move], List[Status])) =>
      (hanoi, res) passes {
        case Hanoi(0, Src, Aux, Dst) =>
          (List(Move(0, Src, Dst)),
            List(Halt))
        case Hanoi(1, Src, Aux, Dst) =>
          (
            List(
  		        Move(0, Aux, Dst),
      	     	  Move(1, Src, Dst),
  			          Move(0, Src, Aux)
    		    ),
    		    List(Wait(5), Halt)
  		    )
        case Hanoi(2, Src, Aux, Dst) => 
          (List(
  			  Move(0, Src, Dst), 
    				Move(1, Aux, Dst),
    				  Move(0, Aux, Src),
      					Move(2, Src, Dst), 
      					  Move(0, Dst, Aux),
        						Move(1, Src, Aux),
        						  Move(0, Src, Dst)
				  ),
				  List(Report("repair"), Wait(10), Halt)
				  )
	  }
  }
  
  def computeStatus(moves: List[Move], previous: List[Status]): List[Status] = 
    List(Halt)
    
  def solve(hanoi: Hanoi): (List[Move], List[Status]) = hanoi match {
    case Hanoi(0, src, _, dst) =>
      (List(Move(0, Src, Dst)), List(Halt))
    case Hanoi(d, src, aux, dst) =>
      val (mv1, st1) = solve(Hanoi(d-1, aux, src, dst))
      val (mv2, st2) = solve(Hanoi(d-1, src, dst, aux))
      val moves = (mv1 :+ Move(d, src, dst)) ++ mv2
      val status = computeStatus(moves, st1 ++ st2)
      (moves, status)
  }
  
//  def main(args: Array[String]): Unit = {
//
//    val input1 = Hanoi(0, Src, Aux, Dst, Initial)
//    assert(Move(0, Src, Dst, Initial) == solve(input1))
//    
//    val input2 = Hanoi(1, Src, Aux, Dst, Initial)
//    assert(
//	  Move(0, Aux, Dst,
//        Move(1, Src, Dst,
//          Move(0, Src, Aux, Initial)
//        )
//      )
//      == solve(input2)
//    )
//	
//    val input3 = Hanoi(2, Src, Aux, Dst, Initial)
//    assert(
//	  Move(0, Src, Dst, 
//		Move(1, Aux, Dst,
//		  Move(0, Aux, Src,
//			Move(2, Src, Dst, 
//			  Move(0, Dst, Aux,
//				Move(1, Src, Aux,
//				  Move(0, Src, Dst, Initial)))))))
//	  == solve(input3)
//    )    
//    
//  }
}