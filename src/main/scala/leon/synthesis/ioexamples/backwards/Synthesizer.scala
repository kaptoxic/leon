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
          assert(mapToSubexps contains fragment, "mapToSubexps(%s) does not contain fragment(%s) for input(%s)"
            .format(mapToSubexps, fragment, input))
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
      
  def explore(root: RootFragment,
    functions: Expr => List[String], 
    subexpressions: Map[Expr, Map[Expr, Expr]],
    inverser: String => Expr => List[List[Expr]]
  ) = {
    fine("Entering explore with " + root)
    
//    type Element = (ExpandableFragment, String)
    type Element = ExpandableFragment
    val queue = m.Queue[Element]()
    
    def process(node: ExpandableFragment): Unit = {
      val step = node.step
      assert(step.getChildren.isEmpty)

      val fragment = node.fragment
      val input = node.input
      val mapToSubexps = subexpressions(input)
      
      fine("mapToSubexps(%s) contains fragment(%s)".format(mapToSubexps, fragment))
      if (mapToSubexps contains fragment) {
        node.setSolved(null)
        node.step.asInstanceOf[SingleStep].solution = mapToSubexps(fragment)
//        node.step.setSolved(null)
      }
      else 
      {
        val applicableOps = functions(fragment)
        fine("applicable functions are: " + applicableOps)
        for (nextOp <- applicableOps) {
          fine("nextOp is " + nextOp)
        
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
          
          val nextToCheck =
            for (expandedNode <- expandedNodes) yield {
              expandedNode match {
                case of: OrFragment => List(of)
                case af: AndFragment => af.getChildren.asInstanceOf[List[ExpandableFragment]]            
              }
            }
          
          node.addChildren(nextOp, expandedNodes)
          
          fine("Enqueueing : " + nextToCheck.flatten)
          queue.enqueue(nextToCheck.flatten: _*)
        }
      }
    }
    
    queue.enqueue(root)
    
    while (!root.isSolved && !queue.isEmpty) {
      val element = queue.dequeue
      
      process(element)
    }
  }
  
  def synthesize(
    inputVar: Identifier,
    examples: List[IO],
    functions: Expr => List[String],
    subexpressions: Map[Expr, Map[Expr, Expr]],
    cst: (String, List[Expr]) => Expr,
    inverser: String => Expr => List[List[Expr]]
  ): Option[Expr] = {
    
    var branches = new m.LinkedList[Branch]
    
    case class Branch(condition: Expr) {
      
      var examples = new m.LinkedList[IO]
      var operationRoot = new RootStep
      var roots = m.Map[IO, RootFragment]()
      
      def getBody = {
        if (operationRoot.isSolved) Some(operationRoot.getSolution(cst))
        else None
      }
      
      def covers(expr: Expr): Boolean = {
        if (condition == BooleanLiteral(true)) true
        else { 
          throw new RuntimeException
//          evaluate(condition, mapping)
        }
      }
      
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
    }
    
    val branch = Branch(BooleanLiteral(true))
    branches :+= branch
    
    val headExample = examples.head
    val fragmentRoot = new RootFragment(branch.operationRoot, headExample)
    branch.examples :+= headExample
    
    explore(fragmentRoot, functions, subexpressions, inverser)
    
    for (ex <- examples.tail) {
      assert(branch.covers(ex._1))
      branch.examples :+= ex
      
      val fragmentRoot = new RootFragment(branch.operationRoot, ex)
      
      propagate(fragmentRoot, subexpressions, inverser)
      val searchFlag = fragmentRoot.isSolved
            
      fine("For example " + ex + " search result is " + searchFlag)
      if (!searchFlag) {        
        explore(fragmentRoot, functions, subexpressions, inverser)
        assert(fragmentRoot.isSolved)
      }
    }
    
    branch.getBody
  }
}