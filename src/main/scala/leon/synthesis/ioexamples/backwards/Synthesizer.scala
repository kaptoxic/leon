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
    fine("stepFun used is: " + step.stepFun)
    
    // for each inverse example build a child
    val inverses = inverser(step.stepFun)(frag)
    val childNodes =
      for (inverse <- inverses) yield {
        
        if (inverse.size > 1) {            
          val andNode = new AndFragment(step, node)
          for (frag <- inverse) {
            val orNode = new OrFragment(step, andNode, frag)
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
      
//  def propagate(root: RootFragment, ex: IO, subexpressions: Map[Expr, Map[Expr, Expr]],
//    inverser: String => Expr => List[List[Expr]]) = {
//    fine("Entering propagate with " + root)
//    
//    def rec(node: Fragment): Unit = node match {
//      case andNode: AndFragment =>
//        assert(!andNode.getChildren.isEmpty)
//        for (child <- andNode.getChildren)
//          rec(child) 
//
//      case orNode: OrFragment =>
//        val solvedNode = orNode.step 
//        val fragment = orNode.fragment
//        val output = ex._1
//        
//        val mapToSubexps = subexpressions(output)
//        if (mapToSubexps contains fragment) {
////          orNode.solved = true
//          orNode.parent.setSolved(orNode)
//        } else {            
//          val childNodes = explore(orNode, opNode.funDef, inverser)
//          
//          // add and explore children
//          orNode.addChildren(solvedNode, childNodes)
//          assert(orNode.getChildren.size == opNode.getChildren.size)
//          for ((child, childOp) <- orNode.getChildren zip opNode.getChildren)
//            rec(child, childOp)
//        }
//    }
//    
//    val operationRoot = root.correspondingNode
//    val solvedNode = operationRoot.solvedNode
//    if (root.getMap contains solvedNode) {
//      // path through solved node already explored
//      rec(root.solvedNode.asInstanceOf[WithExamples], solvedNode)
//    } else {
//      val childNodes = explore(root, "root", inverser)
//      
//      // add and explore children
//      root.addChildren(solvedNode, childNodes)
//      for (child <- childNodes)
//        rec(child, solvedNode)
//    }
//  }
//  
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