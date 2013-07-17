/* Copyright 2009-2013 EPFL, Lausanne */

package leon.synthesis.search

abstract class AndOrGraphSearch[AT <: AOAndTask[S],
                                OT <: AOOrTask[S],
                                S](val g: AndOrGraph[AT, OT, S]) {

  def nextLeaves(): Stream[g.Leaf] = {

    def streamFromAnd(at: g.AndTree): Stream[g.Leaf] = {
      if (!at.isSolved && !at.isUnsolvable) {
        at match {
          case l: g.Leaf =>
            Stream(l)
          case a: g.AndNode =>
            val subTrees = a.subTasks.filterNot(a.subSolutions.keySet).map(a.subProblems)
            subTrees.sortBy(_.minCost).map(streamFromOr _).reduceRight(_ ++ _)
        }
      } else {
        Stream()
      }
    }

    def streamFromOr(ot: g.OrTree): Stream[g.Leaf] = {
      if (!ot.isSolved && !ot.isUnsolvable) {
        ot match {
          case l: g.Leaf =>
            Stream(l)
          case o: g.OrNode =>
            o.alternatives.values.toSeq.sortBy(_.minCost).map(streamFromAnd).reduceRight(_ ++ _)
        }
      } else {
        Stream()
      }
    }

    streamFromOr(g.tree)
  }

  def nextLeaf(): Option[g.Leaf] = nextLeaves().headOption

  abstract class ExpandResult[T <: AOTask[S]]
  case class Expanded[T <: AOTask[S]](sub: List[T]) extends ExpandResult[T]
  case class ExpandSuccess[T <: AOTask[S]](sol: S, isTrustworthy: Boolean) extends ExpandResult[T]
  case class ExpandFailure[T <: AOTask[S]]() extends ExpandResult[T]

  def stop() {

  }

  def search(): Option[(S, Boolean)]

  def onExpansion(al: g.AndLeaf, res: ExpandResult[OT]) {
    res match {
      case Expanded(ls) =>
        al.expandWith(ls)
      case r @ ExpandSuccess(sol, isTrustworthy) =>
        al.isTrustworthy = isTrustworthy
        al.solution = Some(sol)
        al.parent.notifySolution(al, sol)
      case _ =>
        al.isUnsolvable = true
        al.parent.unsolvable(al)
    }

    if (g.tree.isSolved) {
      stop()
    }
  }

  def onExpansion(ol: g.OrLeaf, res: ExpandResult[AT]) {
    res match {
      case Expanded(ls) =>
        ol.expandWith(ls)
      case r @ ExpandSuccess(sol, isTrustworthy) =>
        ol.isTrustworthy = isTrustworthy
        ol.solution = Some(sol)
        ol.parent.notifySolution(ol, sol)
      case _ =>
        ol.isUnsolvable = true
        ol.parent.unsolvable(ol)
    }

    if (g.tree.isSolved) {
      stop()
    }
  }

  def traversePathFrom(n: g.Tree, path: List[Int]): Option[g.Tree] = {
    n match {
      case l: g.Leaf =>
        assert(path.isEmpty)
        Some(l)
      case an: g.AndNode =>
        path match {
          case x :: xs =>
            traversePathFrom(an.subProblems(an.subTasks(x)), xs)
          case Nil =>
            Some(an)
        }

      case on: g.OrNode =>
        path match {
          case x :: xs =>
            val t = on.altTasks(x)
            if (on.triedAlternatives contains t) {
              None
            } else {
              traversePathFrom(on.alternatives(t), xs)
            }

          case Nil =>
            Some(on)
        }
    }
  }

  def traversePath(path: List[Int]): Option[g.Tree] = {
    traversePathFrom(g.tree, path)
  }
}
