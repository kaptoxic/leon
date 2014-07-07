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

class Synthesizer(evaluator: Evaluator, funDef: FunDef,
  termSynthesizer: TypeTree => List[Expr]) extends HasLogger {

  type IO = (Expr, Expr)
  import Util._
  implicit var sortMap = m.Map[IO, Int]()
  
  assert(funDef.args.size == 1)
  val inputVar: Identifier = funDef.args.head.id
  
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
      
  /**
   * @param node fragment node to expand
   * @param stepToExpand step according to which fragment should be expanded
   * @param inverser function that inverses value according to a given function
   * @return expanded child nodes for fragment node
   */
  def expand(node: ExpandableFragment, stepToExpand: Step, inverser: String => Expr => List[List[Expr]]) = {
    fine("Entering explore with " + node)

    val step = stepToExpand//node.step
    assert(step!=null)
//    fine("stepFun used is: " + step.stepFun)
    
    // for each inverse example build a child
    val frag = node.fragment
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
      
  /**
   * propagate fragment tree according to the associated operation tree
   * @param root fragment tree root (references operation tree root)
   * @param subexpressions determines subexpressions of a given expression 
   * @param inverser inverses output values to input values for a function
   * @return map from step nodes to fragment nodes for all leafs in the propagation
   * require: operation tree should be solved
   */
  def propagate(root: RootFragment, subexpressions: Map[Expr, Map[Expr, Expr]],
    inverser: String => Expr => List[List[Expr]]) = {
    assert(root.step.isSolved)
    fine("Entering propagate with " + root)
    
    val mapStepsToFragments = m.Map[Step, ExpandableFragment]()
    
    def rec(node: Fragment): Unit = node match {
      // recursively propagate all children
      case andNode: AndFragment =>
        for (child <- andNode.getChildren)
          rec(child)

      // propagate according to corresponding step
      case orNode: ExpandableFragment =>
        val step = orNode.step
        val fragment = orNode.fragment
        val input = orNode.input
        
        // if no children then this is a leaf
        fine("orNode.step.getChildren.isEmpty, %s".format(orNode.step.getChildren.isEmpty))
        if (orNode.step.getChildren.isEmpty) {
          mapStepsToFragments += (orNode.step -> orNode)
          
      		val mapToSubexps = subexpressions(input)
          fine("orNode.step.isSolved(%s), (mapToSubexps contains fragment)(%s)".format(orNode.step.isSolved, (mapToSubexps contains fragment)))
          fine("mapToSubexps(%s) fragment(%s)".format(mapToSubexps, fragment))

          if (orNode.step.isSolved) {
            // this fragment is a subexpression of input and step node is solved we should set
            // this node as solved
            if((mapToSubexps contains fragment) &&
              orNode.step.asInstanceOf[SingleStep].solution == mapToSubexps(fragment))
              orNode.setSolved(orNode)
            // if not, unsolve the step tree up to the root
            else {
              fine("unsolving " + orNode + " and its step " + orNode.step)
              orNode.step.unsolve(null)
            }
          }
        } else
        { 
          // NOTE: if step has not children and not solved this is still fine
          val childSteps = orNode.step.getChildren
          
          // for all child steps, not already explored, explore them
          for (childStep <- childSteps; stepFun = childStep.stepFun;
        		if ! orNode.getMap.contains(stepFun)) {
            
            val exploredNodes = expand(orNode, childStep, inverser)
            // add expanded children
            orNode.addChildren(stepFun, exploredNodes)
            // propagate recursively
            for ( exploredChild <- exploredNodes )
              rec(exploredChild)
          }
        }
    }
    
    rec(root)

    mapStepsToFragments
  }
  
  /**
   * explores the fragment and step tree and build them at the same time, stops when
   * fragment tree is solved
   * @param root
   * @param functions
   * @param subexpressions
   * @param inverser
   * @param existingQueue queue that is used for traversal
   * @return state of the traversal queue and leaf solved nodes
   */
  def explore(root: RootFragment,
    functions: Expr => List[String], 
    subexpressions: Map[Expr, Map[Expr, Expr]],
    inverser: String => Expr => List[List[Expr]],
    existingQueue: m.Queue[ExpandableFragment]
  ): m.Queue[Step] = {
    fine("Entering explore with " + root)
    
    type Element = ExpandableFragment
    val queue = existingQueue
    val solvedNodesQueue = m.Queue[Step]()
    
    // visit expandable fragment
    def process(node: ExpandableFragment): Unit = {
      fine("process node " + node + ", with step = " + node.step)
      val step = node.step
      assert(step.getChildren.isEmpty, "step(%s) has children(%s)".format(step, step.getChildren))

      val fragment = node.fragment
      val input = node.input
      val mapToSubexps = subexpressions(input)
      
      // current fragment is a subexpression of input
      if (mapToSubexps contains fragment) {
        // fragment and step node are solved        
        node.setSolved(null)
//        assert(!node.step.isSolved)
        node.step.setSolved(null)
        node.step.asInstanceOf[SingleStep].solution = mapToSubexps(fragment)
        fine("setting solution " + mapToSubexps(fragment) + " of " + node.step + " to " + mapToSubexps(fragment))
        
        assert(node.step.getChildren.isEmpty)
        solvedNodesQueue enqueue node.step
      }
      else 
      {
        fine("mapToSubexps(%s) does not contains fragment(%s)".format(mapToSubexps, fragment))
          
        // reset to no longer solved
        fine("setting soution to null " + node.step)
        node.step.unsolve(null)

        // for all applicable functions, expand this fragment
        val applicableOps = functions(fragment)
        fine("applicable functions are: " + applicableOps)
        for (nextOp <- applicableOps) {
          fine("nextOp is " + nextOp)
        
          // get inverses for the function on this fragment
          val inverses = inverser(nextOp)(node.fragment)
          assert(inverses.size >= 1, "should have at least one inverse")
          assert(inverses.map(l => l.size).forall(_ == inverses.head.size), "all inverses should be of the same size")
          
          // create new step node for this function
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
              fine("orNode added with op = " + nextOp)
              val orNode = new OrStep(step, nextOp)
              orNode
            }
          // add this new child step node
          step.addChild(newStepNode)
          // expand fragment node according to the new step node 
          val expandedNodes = expand(node, newStepNode, inverser)
          node.addChildren(nextOp, expandedNodes)
          
          // add all new expandable fragments to the queue to check
          val nextToCheck =
            for (expandedNode <- expandedNodes) yield {
              expandedNode match {
                case of: OrFragment => List(of)
                case af: AndFragment => af.getChildren.asInstanceOf[List[ExpandableFragment]]            
              }
            }
          fine("Enqueueing : " + nextToCheck.flatten)
          queue.enqueue(nextToCheck.flatten: _*)
        }
      }
    }
    
    // traversal
    while (!root.isSolved && !queue.isEmpty) {
      val element = queue.dequeue
      
      process(element)
    }
    fine("exited while root solved is: " + root.isSolved)
    fine("exited while queue is: " + queue.mkString("\n"))
    
    // all solved nodes (search stopped there) and current queue
    solvedNodesQueue ++ queue.map(_.step)
  }
  
  def explore(root: RootFragment,
    functions: Expr => List[String], 
    subexpressions: Map[Expr, Map[Expr, Expr]],
    inverser: String => Expr => List[List[Expr]]    
  ): m.Queue[Step] =
    explore(root, functions, subexpressions, inverser, m.Queue(root))
    
  var branches = new m.LinkedList[Branch]
  
  case class Branch(var condition: Expr) {
    
    var examples = new m.LinkedList[IO]
    var operationRoot = new RootStep
    var roots = m.Map[IO, RootFragment]()
    
    def getBody(cst: (String, List[Expr]) => Expr) = {
      if (operationRoot.isSolved) Some(operationRoot.getSolution(cst))
      else None
    }
    
    def covers(expr: Expr): Boolean = {
      if (condition == BooleanLiteral(true)) true
      else { 
        // NOTE this is hacked
        evaluate(condition, Map(funDef.args.head.id -> expr) ) match {
          case BooleanLiteral(res) => res          
        }
      }
    }
    
  }
  
  def getAllExamples = branches.flatMap( br => br.examples )
  
  /**
   * given a sequence of examples, synthesizes a (recursive) function that explains it
   * @param examples
   * @param functions for each expression, determine which function is applicable (in reverse)
   * @param subexpressions for each input gives a map of subexpressions to functions on that input
   * @param cst constructor function for constructing body of the function, given a function name and
   * expressions it constructs a new expression
   * @param inverser inverses expressions according to given function name
   * @return
   */
  def synthesize(
    examples: List[IO],
    functions: Expr => List[String],
    subexpressions: Map[Expr, Map[Expr, Expr]],
    cst: (String, List[Expr]) => Expr,
    inverser: String => Expr => List[List[Expr]]
  ): Option[Expr] = {
    
    // start with a single branch that covers all examples
    val initialBranch = Branch(BooleanLiteral(true))
    branches :+= initialBranch
    
    // first example and its fragment root
    val headExample = examples.head
    val fragmentRoot = new RootFragment(initialBranch.operationRoot, headExample)
    initialBranch.examples :+= headExample
    
    var operationQueue = m.Queue[Step]()
    var mapStepsToFragments = m.Map[(Step, Expr), ExpandableFragment]()
    
    // explore first example
    operationQueue = explore(fragmentRoot, functions, subexpressions, inverser, 
      m.Queue(fragmentRoot))
    fine("after first, solution: " + fragmentRoot.step.getSolution(cst))
    
    // process other examples
    for (ex <- examples.tail) {
      // TODO determine which branch covers this example
      val currentBranch = branches.head
      assert(currentBranch.covers(ex._1))
    
      // modify inverses function to include recursive call if one of the existing example outputs is seen
      def nextFunctions(e: Expr) = {
        val res =
          e match {        
            case _ if getAllExamples.map(_._2) contains e => "rec" :: functions(e) 
            case _ => functions(e)          
          }        
        fine("nextFunctions for " + e + " = " + res + "\nex is %s, branch examples is (%s), e is %s"
          .format(ex, getAllExamples, e) )
        res
      }
      
      def nextInverses(name: String)(e: Expr): List[List[Expr]] = {
        val res = 
          if (name == "rec" && getAllExamples.map(_._2).contains(e)) {        
            val addEl = getAllExamples.find(_._2 == e).get._1
            List(addEl) :: inverser(name)(e)
          } else inverser(name)(e)
        fine("nextInverses called for " + name + ", " + e + " and got " + res)
        res
      }

      // fragment root for this example
      val fragmentRoot = new RootFragment(currentBranch.operationRoot, ex)
      currentBranch.roots += (ex -> fragmentRoot)
      // propagate the example
      val mapStepsForThisExample =
        propagate(fragmentRoot, subexpressions, nextInverses)
      fine("mapStepsForThisExample is " + mapStepsForThisExample)

      assert(operationQueue.forall(o => mapStepsForThisExample.contains(o)),
        "this keys not found: %s\n".format(operationQueue.toSet.diff(mapStepsForThisExample.keys.toSet).mkString("\n")) +        
        "operationQueue=(%s), while mapStepsForThisExample=(%s)"
          .format(operationQueue.mkString("\n"), mapStepsForThisExample.mkString("\n")))
        
      // check if this example fragment root is solved
      val searchFlag = fragmentRoot.isSolved
      fine("For example " + ex + " search result is " + searchFlag)
      if (!searchFlag) {
        for (step <- operationQueue) {
          assert(step.getChildren.isEmpty, "branch.operationRoot(%s) has children(%s)"
            .format(step, step.getChildren))
          assert( step == mapStepsForThisExample(step).step )
        }
        
        // explore operations for this fragment root
        operationQueue =
          explore(fragmentRoot, nextFunctions, subexpressions, nextInverses,
            operationQueue.map(o => mapStepsForThisExample(o)))
        fine("nextSubexpressions used " + subexpressions)
        assert(fragmentRoot.isSolved, "fragment root not solved for " + fragmentRoot)
        
        // try to propagate other IO pairs
        val propagationVector =
          currentBranch.roots.values map { fragmentRoot =>
            propagate(fragmentRoot, subexpressions, nextInverses)
            fragmentRoot.isSolved
          }
        
        // if some did not propagate, try to find an appropriate branch condition
        if (! propagationVector.forall( identity )) {
          val booleanSnippets = termSynthesizer(BooleanType)
          
          var snippetMatches = false
          for (snippet <- booleanSnippets; if !snippetMatches) {
          	assert(currentBranch.examples.size == propagationVector.size)
          	snippetMatches = propagationVector.zip(currentBranch.examples).forall {
        	    el =>
        	      val (flag, io) = el
        	    	flag == evaluate(snippet, Map(inputVar -> io._1) )
          	}	
          }
        }
//        val (conditionForPreviousBranch, goodExamples, badExamples) = partition(examples)
//        val newBrach = Branch(conditionForPreviousBranch)
//        newBrach.examples ++= goodExamples
////        branches.head.operationRoot
//        branches :+= newBrach
//
//        branches.head.examples = m.LinkedList(badExamples: _*)
        
//        nextSubexpressions = nextSubexpressions.map {
////          case (ex._1, map) => (ex._1, map ++ Map(ex._1 -> (inputVar.toVariable: Expr)))
////          case p => p
//          case (ex, map) =>
//            (ex, map ++ Map(ex._1 -> (inputVar.toVariable: Expr)))
//        }
      }

      currentBranch.examples :+= ex
    }
    
    // TODO fix this
    initialBranch.getBody(cst)
  }
}