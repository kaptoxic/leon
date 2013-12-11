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

object StepGraph {
  
  import AndOrGraph._
       
  trait StepLike {

    def stepFun: String
    
    def getSolution(cst: (String, List[Expr]) => Expr): Expr
    
  }
  
  abstract class Step extends Node[Step] with StepLike {
    
    override def getChildren = super.getChildren.asInstanceOf[List[Step]]
    
  }
  
  abstract class SingleStep extends Step with SingleSolution[Node[Step]] {
    
    override def getSolution(cst: (String, List[Expr]) => Expr): Expr = {
      val innerSolution = getSolvedNode.getSolution(cst)
      cst(stepFun, List(innerSolution))  
    }
    
    override def getSolvedNode = super.getSolvedNode.asInstanceOf[Step]
    
  }
  
  class RootStep() extends SingleStep {    
    
    def stepFun = "root"
      
  }
  
  case class AndStep(parent: Step, override val stepFun: String)
    extends Step with AndNode[Step] {
        
    override def getSolution(cst: (String, List[Expr]) => Expr): Expr =
      cst(stepFun, children.toList.map(_.asInstanceOf[StepLike].getSolution(cst)))          
  }
  
  case class OrStep(parent: Step, var _stepFun: String)
  extends SingleStep with OrNode[Step] {
    
    override def stepFun = _stepFun
    
    def solution_=(sol: Expr) = {
      _solution = sol
      setSolved(null)
    }
    def solution = _solution
    
    private var _solution: Expr = null
    
    def this(parent: Step, solution: Expr) = {
      this(parent, "none")
      _solution = solution
      setSolved(null)
    }
    
    def this(parent: Step, stepFun: String, solution: Expr) = {
      this(parent, stepFun)
      _solution = solution
      setSolved(null)
    }
        
    override def getSolution(cst: (String, List[Expr]) => Expr): Expr = {
      assert(solution != null)
      cst(stepFun, List(solution))  
    }
        
  }
  
//  case class LeafStep(parent: Step, solution: Expr) extends Step {
//    
//    setSolved
//    
//    override val stepFun = "leaf"
//        
//    override def getSolution(cst: (String, List[Expr]) => Expr): Expr =
//      cst(stepFun, List(solution))
//  }

}