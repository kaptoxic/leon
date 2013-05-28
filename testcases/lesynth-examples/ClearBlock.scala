import leon.Utils._

object ClearBlock {
  
//  case class Block
  // NOTE this is done ad-hoc for this example since val a = Block, b = Block, a==b in Scala if Block is case class
  sealed abstract class Block
  case object A extends Block
  case object B extends Block
  case object C extends Block
		  
  sealed abstract class Tower
  case object Table extends Tower
  case class BlockTower(block: Block, tower: Tower) extends Tower
  
  abstract class State
  case object Initial extends State  
  case class Put(block: Block, state: State) extends State
  
//  def clearBlock(block: Block, tower: Tower, state: State): State = tower match {
//    case Table => state
//    case BlockTower(topBlock, innerTower) if block == topBlock =>
//      state
//    case BlockTower(topBlock, innerTower) =>
//      clearBlock(block, innerTower, Put(topBlock, state))
//  }
    
  def clearBlockSyn(block: Block, tower: Tower, state: State): State = {
//    val a = new Block
//    val b = new Block
//    val c = new Block
    choose {
	    (res: State) =>
	      passes(
		    Map[(Block, Tower, State), State](
	        (A, Table, Initial) -> Initial,
		      (A, BlockTower(A, Table), Initial) -> Initial,
		      (A, BlockTower(A, BlockTower(B, Table)), Initial) -> Initial,
		      (A, BlockTower(B, BlockTower(A, Table)), Initial) -> Put(B, Initial),
		      (A, BlockTower(C, BlockTower(B, BlockTower(A, Table))), Initial) -> Put(B, Put(C, Initial))	      
	      ),
	      (block, tower, state), res
		  )
    }
  }
  
}