import scala.collection.immutable.Set
import leon.Annotations._
import leon.Utils._

object BinaryTree {
  sealed abstract class Tree
  case class Node(left : Tree, value : Int, right : Tree) extends Tree
  case class Leaf() extends Tree

  def content(t : Tree): Set[Int] = t match {
    case Leaf() => Set.empty[Int]
    case Node(l, v, r) => content(l) ++ Set(v) ++ content(r)
  }
  
  sealed abstract class OptInt
  case class Some(value : Int) extends OptInt
  case object None extends OptInt
  
  def isSortedX(t : Tree) : (Boolean, OptInt, OptInt) = (t match {
    case Leaf() => (true, None, None)
    
    case Node(l, v, r) =>
      val (ls,ll,lu) = isSortedX(l)
      val (rs,rl,ru) = isSortedX(r)
            
      val (lOK,newMin) = lu match {
        case None => (ls, v)
        case Some(v2) => (ls && v2 < v, v2)
      }
      
      val (rOK,newMax) = rl match {
        case None => (rs,v)
        case Some(v2) => (rs && v < v2, v2)
      }
      
      (lOK && rOK, Some(newMin), Some(newMax))
  })

  def isSorted(t: Tree): Boolean = isSortedX(t)._1

  def deleteSynth(in : Tree, v : Int) = choose {
    (out : Tree) => content(out) == (content(in) -- Set(v))
  }

  def insertSynth(in : Tree, v : Int) = choose {
    (out : Tree) => content(out) == (content(in) ++ Set(v))
  }

  def insertSortedSynth(in : Tree, v : Int) = choose {
    (out : Tree) => isSorted(in) && (content(out) == (content(in) ++ Set(v))) && isSorted(out)
  }

  def deleteSortedSynth(in : Tree, v : Int) = choose {
    (out : Tree) => isSorted(in) && (content(out) == (content(in) -- Set(v))) && isSorted(out)
  }
}
