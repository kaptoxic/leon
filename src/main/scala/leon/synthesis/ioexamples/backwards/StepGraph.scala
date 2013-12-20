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
      assert(solution != null || getSolvedNode != null, "getSolvedNode and solution is null at " + this)
        
      if (solution == null) {
        val innerSolution = getSolvedNode.getSolution(cst)
        cst(stepFun, List(innerSolution))
      } else
        solution
    }
    
    override def getSolvedNode = super.getSolvedNode.asInstanceOf[Step]
    
    def solution_=(sol: Expr) = {
      _solution = sol
      setSolved(null)
    }
    def solution = _solution
    
    protected var _solution: Expr = null
    
  }
  
  class RootStep() extends SingleStep {    
    
    def stepFun = "root"
      
  }
  
  class AndStep(override val parent: Step, override val stepFun: String)
    extends Step with AndNode[Step] {
        
    override def getSolution(cst: (String, List[Expr]) => Expr): Expr =
      cst(stepFun, children.toList.map(_.asInstanceOf[StepLike].getSolution(cst)))          
  }
  
  class OrStep(override val parent: Step, var _stepFun: String = "none")
  extends SingleStep with OrNode[Step] {
    
    override def stepFun = _stepFun
    
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
    
    override def solution_=(sol: Expr) = {      
      _solution = sol
      setSolved(null)
    }

    override def toString = "OrStep - " + stepFun + " solution= " + solution + "[" + this.##
        
//    override def getSolution(cst: (String, List[Expr]) => Expr): Expr = {
//      assert(solution != null)
//      cst(stepFun, List(solution))  
//    }
        
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