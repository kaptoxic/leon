import leon.lang.{ Map => _, _ }
import leon.lang.synthesis._
import leon.annotation._
import leon.collection._
import leon.io._

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
  case class VertexInner(edges : List[Edge]) extends Vertex
  case class VertexEnd(info: SourceInfo) extends Vertex
  case class Transformed(info: TargetInfo) extends Vertex
//  sealed abstract class Edge
//  case class EdgeItem(toVertex : Vertex, weight : Int) extends Edge
//  case class EdgeEnd() extends Edge
  case class Edge(toVertex : Vertex, weight : Int)
  
  def preorder(v: Vertex): List[(Vertex, List[Edge])] = {
    
    def rec(innerV: Vertex, currPath: List[Edge]): List[(Vertex, List[Edge])] = {
      
      innerV match {
        case VertexEnd(_) =>
          (innerV, currPath) :: Nil()
        case VertexInner(edges) =>
          val res =
            for (edge <- edges)
              yield rec(edge.toVertex, currPath :+ edge)
          ListOps.flatten(res)
      }
      
    }
    
    rec(v, Nil())
    
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
  
  def test {
    
    val v1 = VertexEnd(A)
    
    val e21 = Edge(v1, 1)
    val v2 = VertexInner(List(e21))
    
    assert( preorder(v1) == (v1, Nil[Edge]()) :: Nil[(Vertex, List[Edge])]() )
    assert( preorder(v1) == List[(Vertex, List[Edge])]((v1, Nil[Edge]())) )

    assert( preorder(v2) == List[(Vertex, List[Edge])]((v1, List[Edge](e21))) )
    
    val v1t = Transformed(D)
    
    assert( v1t == Transformed(D) )
    assert( solve(v1) == v1t )
    
  }

}