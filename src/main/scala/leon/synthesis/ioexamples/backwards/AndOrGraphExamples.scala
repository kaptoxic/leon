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

object FragmentGraph {
  
  import AndOrGraph._
  import StepGraph._
  
  trait WithExamples {

    def input: Expr
    
  }
  
  abstract class Fragment(val step: Step) extends Node[Fragment] with WithExamples {
    
    override def toString = "Fr{" + step + "}"
    
    override def getChildren = super.getChildren.asInstanceOf[List[Fragment]]
    
  }
  
  // only OR or Root node can be expanded to new children
  abstract class ExpandableFragment(override val step: Step, val fragment: Expr)
    extends Fragment(step) with SingleSolution[Node[Fragment]] {
    
    override def toString = super.toString + "(" + fragment + ")"

    protected var childrenMap = m.Map[String, List[Fragment]]()
    
    def getMap = childrenMap
    
    def addChildren(step: String, children: List[Fragment]) {
      childrenMap += (step -> children)
      
      for (child <- children)
        super.addChild(child)
    }
    
  }
  
  class RootFragment(override val step: Step, override val input: Expr, val output: Expr)
    extends ExpandableFragment(step, output) {
    
    def this(step: Step, ex: (Expr, Expr)) =
      this(step, ex._1, ex._2)
     
  }
 
  class AndFragment(override val step: Step, override val parent: Fragment) extends Fragment(step)
    with AndNode[Fragment] {
    
    override def input = parent.input
    
  }
    
  class OrFragment(override val step: Step, override val parent: Fragment, override val fragment: Expr)
    extends ExpandableFragment(step, fragment) with OrNode[Fragment] {
    
    override def input = parent.input
  }

}