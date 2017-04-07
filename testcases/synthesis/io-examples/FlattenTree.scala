import leon.lang._
import leon.lang.synthesis._

object Flatten {
  
  case class Pair(fst: Int, snd: Int)

  sealed abstract class Tree
  case class Node(left: Tree, value: Pair, right: Tree) extends Tree
  case object Nil extends Tree
  
  sealed abstract class IntTree
  case class IntNode(left: IntTree, value: Int, right: IntTree) extends IntTree
  case object IntNil extends IntTree

  def flatten(l: Tree): IntTree = choose {
    (out: IntTree) => (l, out) passes {
      case Nil => IntNil
      case Node(Nil, Pair(1, 2), Nil) => IntNode(IntNode(IntNil, 1, IntNil), 2, IntNil)
      case Node(Nil, Pair(1, 2), Node(Nil, Pair(3, 4), Nil)) =>
        IntNode(IntNode(IntNil, 1, IntNil), 2, IntNode(IntNode(IntNil, 3, IntNil), 4, IntNil))
    }
  }

}
