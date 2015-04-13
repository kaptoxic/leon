object BSTSimpler {
  sealed abstract class Tree
  case class Node(left: Tree, value: Int, right: Tree) extends Tree
  case class Leaf() extends Tree

  def size(t : Tree) : Int = (t match {
    case Leaf() => 1
    case Node(l,_,r) => size(l) + 1 + size(r)
  }) ensuring(_ >= 1)

  def content(tree: Tree): Set[Int] = tree match {
    case Leaf() => Set.empty[Int]
    case Node(l, v, r) => content(l) ++ Set(v) ++ content(r)
  }
}

