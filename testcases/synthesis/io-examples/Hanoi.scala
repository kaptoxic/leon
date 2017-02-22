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
		  
  def solveArray(hanoi: Hanoi): List[Move] = choose {
    (res: List[Move]) =>
      (hanoi, res) passes {
        case Hanoi(0, Src, Aux, Dst) =>
          List(Move(0, Src, Dst))
        case Hanoi(1, Src, Aux, Dst) =>
          List(
  		         Move(0, Aux, Dst),
      	     	  Move(1, Src, Dst),
  			         Move(0, Src, Aux)
    		     )
        case Hanoi(2, Src, Aux, Dst) => 
          List(
  			  Move(0, Src, Dst), 
    				Move(1, Aux, Dst),
    				  Move(0, Aux, Src),
      					Move(2, Src, Dst), 
      					  Move(0, Dst, Aux),
        						Move(1, Src, Aux),
        						  Move(0, Src, Dst)
				  )
	  }
  }
    
//  def solve(hanoi: Hanoi): State = hanoi match {
//    case Hanoi(0, src, _, dst, state) => Move(0, src, dst, state)
//    case Hanoi(d, src, aux, dst, state) =>
//      solve(Hanoi(d-1, aux, src, dst,
//        Move(d, src, dst,
//    	  solve(Hanoi(d-1, src, dst, aux, state))
//		)
//	  ))
//  }
  
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