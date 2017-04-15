import leon.lang.{ Map => _, _ }
import leon.lang.synthesis._
import leon.annotation._
import leon.collection._
//import leon.io._

object GraphStrategy {
 
  sealed abstract class SourceInfo 
  case object A extends SourceInfo
  case object B extends SourceInfo
  case object C extends SourceInfo
 
  sealed abstract class TargetInfo
  case object D extends TargetInfo
  case object E extends TargetInfo
  case object F extends TargetInfo
  
  sealed abstract class Vertex
  case class VertexInner(edges : List[Edge]) extends Vertex {
    @ignore
    override def toString: String = {
      " " + List.mkString[Vertex](edges.map(_.toVertex), ",", _.toString)
    }
  }
  case class VertexEnd(info: SourceInfo) extends Vertex {
    @ignore
    override def toString: String = {
      "{" + info + "}"
    }
  }
  case class Transformed(info: TargetInfo) extends Vertex {
    @ignore
    override def toString: String = {
      "[" + info + "]"
    }
  }
  sealed abstract class EdgeAbs
//  case class EdgeItem(toVertex : Vertex, weight : Int) extends Edge
//  case class EdgeEnd() extends Edge
  case class Edge(toVertex : Vertex, weight : Int) extends EdgeAbs
  
  def preorder(v: Vertex): List[(Vertex, List[Edge])] = {
    def rec(innerV: Vertex, currPath: List[Edge], visited: Set[Vertex]):
      (List[(Vertex, List[Edge])], Set[Vertex]) = {
      innerV match {
        case _ if visited.contains(innerV) =>
          (Nil(), visited)
        case VertexEnd(_) =>
          ((innerV, currPath) :: Nil(), visited + innerV)
        case VertexInner(edges) =>
          val res =
            edges.foldLeft((List[(Vertex, List[Edge])](), visited)) {
              case ((resList, resVisited), edge) =>
                val (newEl, newVisited) = rec(edge.toVertex, currPath :+ edge, resVisited + innerV)
                (resList ++ newEl, newVisited)
            }
//          ListOps.flatten(res)
          res
      }
    }
    
    val (res, _) = rec(v, Nil(), Set())
    res
  }
  
  def solve(v: Vertex): Vertex = {
    def rec(innerV: Vertex): Vertex = {
      innerV match {
        case VertexEnd(A) =>
          Transformed(D)
        case VertexEnd(B) =>
          Transformed(E)
        case VertexEnd(C) =>
          Transformed(F)
        case VertexInner(edges) =>
          VertexInner(edges map { e => Edge(rec(e.toVertex), e.weight) })
      }
    }
    
    rec(v)
  }

  def solve(v: Vertex, fin: Vertex): Vertex = {
    def rec(innerV: Vertex): Vertex = {
      innerV match {
        case VertexEnd(A) if innerV == fin =>
          Transformed(D)
        case VertexEnd(B) if innerV == fin =>
          Transformed(E)
        case VertexEnd(C) if innerV == fin =>
          Transformed(F)
        case VertexInner(edges) =>
          VertexInner(edges map { e => Edge(rec(e.toVertex), e.weight) })
        case _ => innerV
      }
    }
    
    rec(v)
  }
  
//  def solveContinue(v: Vertex, path: List[Vertex]): Vertex = {
//    def rec(innerV: Vertex, innerPath: List[Vertex]): Vertex = {
//      (innerPath, innerV) match {
//        case (Nil(), VertexEnd(A)) =>
//          Transformed(D)
//        case (Nil(), VertexEnd(B)) =>
//          Transformed(E)
//        case (Nil(), VertexEnd(C)) =>
//          Transformed(F)
//        case (next :: rest, VertexInner(edges)) =>
//          VertexInner(edges map { e =>
//            if (e.toVertex == next)
//              Edge(rec(e.toVertex, rest), e.weight)
//            else e
//          })
//      }
//    }
//    
//    rec(v, path)
//  }
  
  val v1 = VertexEnd(A)
  val e31 = Edge(v1, 3)
  val v4 = VertexEnd(B)
  val e34 = Edge(v4, 6)
  val v6 = VertexEnd(C)
  val e36 = Edge(v6, 11)

  val v3 = VertexInner(List(e31, e34, e36))
  
  val v1t = Transformed(D)
  val v4t = Transformed(E)
  val v6t = Transformed(F)
  val e34t = Edge(v4t, 6)
  val e31t = Edge(v1t, 3)
  val e36t = Edge(v6t, 11)
  val v3t = VertexInner(List(e31t, e34t, e36t))
 
  val v3t1 = VertexInner(List(e31t, e34, e36))
  val e36t1 = Edge(v3t1, 7)

  val v3t2 = VertexInner(List(e31t, e34t, e36))
  val e36t2 = Edge(v3t2, 7)

  def problem(in: Vertex) = choose {
    (out : Vertex) => (in, out) passes Map (
      v3 -> v3t1,
      v3t1 -> v3t2,
      v3t2 -> v3t
    )
  }
  
  def test = {
  
  }

}