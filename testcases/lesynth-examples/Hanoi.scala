import leon.Utils._

object HanoiObject {
  
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
		  
  def solve(hanoi: Hanoi) = choose {
    (res: State) =>
      passes(
	    Map[Hanoi, State](
          Hanoi(0, Src, Aux, Dst, Initial) -> Move(0, Src, Dst, Initial),
          Hanoi(1, Src, Aux, Dst, Initial) ->
    		    Move(0, Aux, Dst,
        		  Move(1, Src, Dst,
    			    Move(0, Src, Aux, Initial)
    			  )
    		),
          Hanoi(2, Src, Aux, Dst, Initial) -> 
			  Move(0, Src, Dst, 
				Move(1, Aux, Dst,
				  Move(0, Aux, Src,
					Move(2, Src, Dst, 
					  Move(0, Dst, Aux,
						Move(1, Src, Aux,
						  Move(0, Src, Dst, Initial)))))))
        ),
        hanoi, res
	  )
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