import leon.lang.{ Map => _, _ }
import leon.lang.synthesis._
import leon.annotation._
import leon.collection._

object GraphStrategy {
  
//  sealed abstract class VertexAbs
  case class Vertex(edges : List[Edge])
//  case class VertexEnd() extends VertexAbs
//  sealed abstract class Edge
//  case class EdgeItem(toVertex : Vertex, weight : Int) extends Edge
//  case class EdgeEnd() extends Edge
  case class Edge(toVertex : Vertex, weight : Int)
  
  def preorder(v: Vertex): List[(Vertex, List[Edge])] = {
    
    def rec(innerV: Vertex, currPath: List[Edge]): List[(Vertex, List[Edge])] = {
      
      innerV match {
        case Vertex(Nil()) =>
          (innerV, currPath) :: Nil()
        case Vertex(edges) =>
          val res =
            for (edge <- edges)
              yield rec(edge.toVertex, currPath :+ edge)
          ListOps.flatten(res)
      }
      
    }
    
    rec(v, Nil())
    
  }
  
  def test: Unit = {
    
    val v1 = Vertex(Nil())
    
    val e21 = Edge(v1, 1)
    val v2 = Vertex(e21 :: Nil())
    
    assert( preorder(v1) == (v1, Nil[Edge]()) :: Nil[(Vertex, List[Edge])]() )

    assert( preorder(v2) == List(v1, List(e21)) )
    
  }

}