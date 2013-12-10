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

object AndOrGraphExamples {
  
  import AndOrGraph._
  
  trait WithExamples extends Solvable {

    override def getChildren = children.toList.asInstanceOf[List[WithExamples]]
//    var output: Expr = _
  }
  
  trait ExampleOr extends WithExamples {
    var fragment: Expr = _
//    var correspondingNode: SingleSolution = _
    protected var childrenMap = m.Map[Step, List[WithExamples]]()
    
    def getMap = childrenMap
    
    def addChildren(step: Step, children: List[WithExamples]) {
      childrenMap += (step -> children)
      for (child <- children)
        super.addChild(child.asInstanceOf[Node])
    }
  }
  
  trait SingleSolutionWithExample extends SingleSolution with ExampleOr
  
  class ExampleRoot(val correspondingNode: Root) extends SingleSolutionWithExample
  
//  class ExampleNode(parent: Step, funDef: String) extends Node(parent, funDef) with WithExamples
  class ExampleAndNode(parent: Step, funDef: String) extends AndNode(parent, funDef) with WithExamples
  class ExampleOrNode(parent: Step, funDef: String) extends OrNode(parent, funDef) with SingleSolutionWithExample

}