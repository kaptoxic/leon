object ClearBlock {
  
  class Block
		  
  sealed abstract class Tower
  case object Table extends Tower
  case object BlockTower(block: Block, tower: Tower) extends Tower
  
  abstract class State
  case object Initial extends State
  case class Move(disks: Int, src: Peg, dst: Peg, state: State) extends State
  
  case class Hanoi(disks: Int, src: Peg, aux: Peg, dst: Peg, state: State)
  
	  def dec(value: Int) = value - 1
    
  def isZero(value: Int) = value == 0
    
  def solve(hanoi: Hanoi): State = choose {
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
}