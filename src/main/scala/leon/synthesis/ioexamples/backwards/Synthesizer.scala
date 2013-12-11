package leon
package synthesis.ioexamples
package backwards

import scala.collection.{ mutable => m }

import purescala.Trees._
import purescala.TypeTrees._
import purescala._
import purescala.Definitions._
import purescala.Common._
import evaluators._

import insynth.util.logging.HasLogger

class Synthesizer(evaluator: Evaluator) extends HasLogger {

  type IO = (Expr, Expr)
  import Util._
  implicit var sortMap = m.Map[IO, Int]()
  
  // and/or graphs
  import AndOrGraph._
  import StepGraph._
  import FragmentGraph._
  
  def evaluate(expr: Expr, mapping: Map[Identifier, Expr]) = {
    import EvaluationResults._
    
    val res =
      evaluator.eval(expr, mapping) match {
        case Successful(res) =>
          res
        case m =>
          throw new Exception("Evaluation problem")
      }
    info("Eval res is: " + res)
    
    res
  }
      
  def expand(node: ExpandableFragment, stepToExpand: Step, inverser: String => Expr => List[List[Expr]]) = {
    fine("Entering explore with " + node)
    // explore the path
//    val funDef = node.correspondingNode.solvedNode.funDef
    val frag = node.fragment
    val step = stepToExpand//node.step
    assert(step!=null)
//    fine("stepFun used is: " + step.stepFun)
    
    // for each inverse example build a child
    fine("Calling inverser with: %s and %s".format(step.stepFun, frag.toString))
    val inverses = inverser(step.stepFun)(frag)
    val childNodes =
      for (inverse <- inverses) yield {
        
        if (inverse.size > 1) {            
          val andNode = new AndFragment(step, node)
          assert(stepToExpand.isInstanceOf[AndStep], "stepToExpand(%s) is not AndStep".format(stepToExpand))
          // if not the same size that means that node should not be explored
//          assert(inverse.size == stepToExpand.getChildren.size)
          for ((frag, innerStep) <- inverse zip stepToExpand.getChildren) {
            val orNode = new OrFragment(innerStep, andNode, frag)
            andNode addChild orNode
          }
          
          andNode
        }
        else {
          val orNode = new OrFragment(step, node, inverse.head)
          orNode
        }
      }
    
    childNodes
  }
      
  def propagate(root: RootFragment, subexpressions: Map[Expr, Map[Expr, Expr]],
    inverser: String => Expr => List[List[Expr]]) = {
    fine("Entering propagate with " + root)
    
    def rec(node: Fragment): Unit = node match {
      case andNode: AndFragment =>
//        assert(!andNode.getChildren.isEmpty)
        for (child <- andNode.getChildren)
          rec(child) 

      case orNode: ExpandableFragment =>
        val step = orNode.step 
        val fragment = orNode.fragment
        val input = orNode.input
        
        val mapToSubexps = subexpressions(input)
        // if no children then this is a leaf
        fine("orNode " + orNode)
        fine("orNode.step.getChildren.isEmpty, orNode.step.isSolved= %s, %s".format(orNode.step.getChildren.isEmpty, orNode.step.isSolved))
        if (orNode.step.getChildren.isEmpty && orNode.step.isSolved) {
          fine("orNode.step.isSolved = true, for " + orNode.step)
          assert(mapToSubexps contains fragment)
          fine(mapToSubexps + " contains " + fragment)
//          assert(orNode.step.isSolved)
//          assert(orNode.step.asInstanceOf[SingleStep].getSolvedNode.isInstanceOf[LeafStep],
//            "node is: " + orNode + ", while step is " + orNode.step)
          assert(orNode.step.asInstanceOf[SingleStep].solution == mapToSubexps(fragment),
            "found solutions do not match - %s and %s".format(
              orNode.step.asInstanceOf[SingleStep].solution, mapToSubexps(fragment)
            ))

          orNode.setSolved(orNode)          
        } else
        {            
          val childSteps = orNode.step.getChildren
          
          // for all steps, not already explored, explore them
          for (childStep <- childSteps; stepFun = childStep.stepFun; if ! orNode.getMap.contains(stepFun)) {
            
            val exploredNodes = expand(orNode, childStep, inverser)
            
            orNode.addChildren(stepFun, exploredNodes)
            for ( exploredChild <- exploredNodes )
              rec(exploredChild)
          }
        }
    }
    
    rec(root)
  }
      
  def explore(root: RootFragment, functions: Expr => List[String], 
    subexpressions: Map[Expr, Map[Expr, Expr]],
    inverser: String => Expr => List[List[Expr]]) = {
    fine("Entering explore with " + root)
    
    type Element = (ExpandableFragment, String)
    val queue = m.Queue[Element]()
    
    def process(node: ExpandableFragment, nextOp: String): Unit = {
      val step = node.step
      assert(step.getChildren.isEmpty)

      val fragment = node.fragment
      val input = node.input
      val mapToSubexps = subexpressions(input)
      
      if (mapToSubexps contains fragment) {
        node.setSolved(null)
        node.step.asInstanceOf[SingleStep].solution = mapToSubexps(fragment)
        node.step.setSolved(null)
      }
      
      val inverses = inverser(nextOp)(node.fragment)
      assert(inverses.size >= 1)
      assert(inverses.map(l => l.size).forall(_ == inverses.head.size))
      
      val newStepNode =
        if (inverses.head.size > 1) {            
          val andNode = new AndStep(step, nextOp)
          for (_ <- inverses.head) {
            val orNode = new OrStep(andNode)
            andNode addChild orNode
          }
          
          andNode
        }
        else {
          val orNode = new OrStep(step, nextOp)
          orNode
        }
      
      step.addChild(newStepNode)
        
      val expandedNodes = expand(node, newStepNode, inverser)
    }
    
    while (!root.isSolved && !queue.isEmpty) {
      val element = queue.dequeue
      
      process(element._1, element._2)
    }
  }
  
//  def synthesize(
//    inputVar: Identifier,
//    examples: List[IO],
//    functions: Map[TypeTree, Seq[FunDef]],
//    decomposer: Expr => Map[Expr, Expr],
//    inverser: FunDef => Expr => List[List[Expr]]): Option[Expr] = {
//    
//    var subexpressions = m.Map[Expr, Map[Expr, Expr]]()
//    
//    var branches = new m.LinkedList[Branch]
//    
//    case class Branch(condition: Expr) {
//      
//      var examples = new m.LinkedList[IO]
//      var operationRoot = new Root
//      var roots = m.Map[IO, ExampleRoot]()
//      
//      def addExample(ex: IO) = {
//        examples :+= ex
//
//        val thisRoot = new ExampleRoot(operationRoot)
////        thisRoot.output = ex._2
//        thisRoot.fragment = ex._2
////        thisRoot.correspondingNode = operationRoot
//        
//        roots += (ex -> thisRoot)
//      }
//      
//      var nodes = m.Queue[Step](Root)
//      
//      def getBody = {
//        if (nodes.head.isSolved) Some(nodes.head.getSolution)
//        else None
//      }
//      
//      def search: Boolean = {        
//        var found = false
//        
//        while (!found && !nodes.isEmpty) {
//          val headNode = nodes.head
//          
//          found =
//            examples.forall( ex =>
//              evaluate(headNode.expr, Map(inputVar -> ex._1)) == ex._2
//            )
//            
//          headNode.resolved = found          
//          
//          if(!found) {
//            nodes.dequeue
//            // for all applicable inverses
//            for(fun <- functions())
//          }
//        }
//        
//        found
//      }
//    }
//    
//    val headExample = examples.head
//    
//    val branch = Branch(BooleanLiteral(true))
//    branch.examples :+= headExample
//    
//    val initialNode = Node(inputVar.toVariable, List(inputVar.toVariable))
//    branch.nodes enqueue initialNode
//    initialNode.resolved = true
//    
//    branches :+= branch
//    
//    for (ex <- examples.tail) {
//      branch.examples :+= ex
//      val searchFlag = branch.search
//      fine("For example " + ex + " search result is " + searchFlag)
//      if (!searchFlag) return None
//    }
//    
//    branch.getBody
//    null
//  }
}