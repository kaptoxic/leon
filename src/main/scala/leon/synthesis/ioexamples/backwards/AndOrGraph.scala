package leon
package synthesis
package ioexamples.backwards

import scala.collection.{ mutable => m }

import purescala.Trees._
import purescala.TypeTrees._
import purescala._
import purescala.Definitions._
import purescala.Common._
import evaluators._

object AndOrGraph {
  
  trait Step {
    protected var children = m.LinkedList[Node]()
    
    def addChild(child: Node) = children :+= child
    
    def getChildren: List[Step] = children.toList
    
    def isSolved = solved
    
    def getSolution(cst: (String, List[Expr]) => Expr): Expr
    
    def funDef: String

    private var solved = false
    
    def setSolved(n: Node) = {
      solved = true
    }
    
    def setSolved = {
      solved = true
    }
  }
  
  trait SingleSolution extends Step {
    
    override def getSolution(cst: (String, List[Expr]) => Expr) =
      solvedNode.getSolution(cst: (String, List[Expr]) => Expr)
            
    var solvedNode: Node = _
    
    override def setSolved(n: Node) = {
      super.setSolved(n)
      solvedNode = n
    }
  }
  
  class Root() extends SingleSolution {    
    def funDef = "root"
  }
  
  abstract class Node(parent: Step, val funDef: String) extends Step {
    
  }
  
  case class AndNode(parent: Step, override val funDef: String) extends Node(parent, funDef) {
    var numberOfSolved = 0
    
    override def setSolved(n: Node) = {
      super.setSolved(n)
      numberOfSolved += 1
      if (numberOfSolved == children.size)
        parent.setSolved(this)
    }
    
    override def getSolution(cst: (String, List[Expr]) => Expr): Expr =
      cst(funDef, children.toList.map(_.getSolution(cst)))
          
  }
  
  case class OrNode(parent: Step, override val funDef: String) extends Node(parent, funDef) with SingleSolution {
        
    override def setSolved(n: Node) = {
      super.setSolved(n)
      parent.setSolved(this)
    }
  }
  
  case class LeafNode(parent: Step, solution: Expr) extends Node(parent, "leaf") {
        
    override def getSolution(cst: (String, List[Expr]) => Expr): Expr =
      solution
  }

}