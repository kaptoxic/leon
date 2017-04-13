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
  val e21 = Edge(v1, 1)
  val v2 = VertexInner(List(e21))
  val e31 = Edge(v1, 3)
  val v4 = VertexEnd(B)
  val e34 = Edge(v4, 6)
  val v3 = VertexInner(List(e31, e34))
  
  val e53 = Edge(v3, 7)
  val e52 = Edge(v2, 13)
  val v5 = VertexInner(List(e52, e53))
  
  // ----------------
  
  val v1t = Transformed(D)
  val e21t = Edge(v1t, 1)
  val v2t = VertexInner(List(e21t)) 
  val v4t = Transformed(E)
  val e34t = Edge(v4t, 6)
  val e31t = Edge(v1t, 3)
  val v3t = VertexInner(List(e31t, e34t))
 
  val e53t = Edge(v3t, 7)
  val e52t = Edge(v2t, 13) 
  val v5t = VertexInner(List(e52t, e53t)) 
  
  // after first step
  
  val v3t1 = VertexInner(List(e31t, e34))
 
  val e53t1 = Edge(v3t1, 7)
  val v5t1 = VertexInner(List(e52t, e53t1)) 

  def problem(in: Vertex) = choose {
    (out : Vertex) => (in, out) passes Map (
      v5 -> v5t1,
      v5t1 -> v5t
    )
  }
  
  def test = {
  
    assert( preorder(v1) == (v1, Nil[Edge]()) :: Nil[(Vertex, List[Edge])]() )
    assert( preorder(v1) == List[(Vertex, List[Edge])]((v1, Nil[Edge]())) )
  
    assert( preorder(v2) == List[(Vertex, List[Edge])]((v1, List[Edge](e21))) )
    
    assert( v1t == Transformed(D) )
    assert( solve(v1) == v1t ) 
    assert( solve(v2) == v2t )

    assert( solve(v5) == v5t )
    
    val preorderList = preorder(v5) 

    assert( solve(v5, preorderList.head._1) == v5t1 )
    assert( solve(v5t1, preorderList(1)._1) == v5t )

//    solve(v5t1, preorderList(1)._1)
//    preorderList(1)._1
    
  }

}