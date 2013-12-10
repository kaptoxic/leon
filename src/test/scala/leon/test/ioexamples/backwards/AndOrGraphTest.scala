package leon.test
package ioexamples.backwards

import leon.purescala.Definitions._
import leon.purescala.Trees._
import leon.purescala.TreeOps._
import leon.solvers.z3._
import leon.solvers.Solver
import leon.synthesis.{ Synthesizer => _, _ }
import leon.synthesis.utils._
import leon.synthesis.ioexamples.backwards._
import leon.evaluators._

import leon.test.condabd.util._

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import java.io.{ BufferedWriter, FileWriter, File }

class AndOrGraphTest extends FunSuite with ShouldMatchers {

  import Scaffold._
  import AndOrGraph._
  import AndOrGraphExamples._

  val testDir = "testcases/ioexamples/"
    
  val problems = forFile(testDir + "MergeSort_Sort.scala").toList
  problems.size should be (1)
    
  type IO = (Expr, Expr)
  // may come in handy
  implicit def exprToPair(expr: Expr) = (expr, expr)
    
  {
    val (sctx, funDef, problem) = problems.head
    
    val program = sctx.program
    val inputVar = funDef.args.head.toVariable
    val codeGenEval = new CodeGenEvaluator(sctx.context, sctx.program)
    
    val consClass = program.caseClassDef("Cons")
    val nilClass = program.caseClassDef("Nil")
    val nilExp = CaseClass(nilClass, Nil): Expr
    
    val functions = program.definedFunctions.groupBy( f => f.returnType)
    
    val nil = nilExp
    val l1 = (CaseClass(consClass, IntLiteral(1) :: nilExp :: Nil))
    
    val synthesizer = new Synthesizer(codeGenEval)
    
    test("solving without and nodes") {
      val root = new Root()
      val consNode = new OrNode(root, "Cons")
      root addChild consNode        
      val consNode_1 = new LeafNode(consNode, inputVar)
      consNode_1 addChild consNode
      
      val mergeNode = new AndNode(root, "merge")
      root addChild mergeNode
      val m1Node = new LeafNode(mergeNode, inputVar)
      mergeNode addChild m1Node
      val m2Node = new OrNode(mergeNode, "sameList")
      mergeNode.addChild(m2Node)
      val m21Node = new LeafNode(m2Node, inputVar)
      m2Node.addChild(m21Node)
              
      consNode.setSolved(consNode_1)
      
      m2Node.isSolved should be (false)
      mergeNode.isSolved should be (false)
      root.isSolved should be (true)
      root.solvedNode should not be (null)
      consNode.isSolved should be (true)
      consNode.solvedNode should not be (null)
    }
    
    test("solving with and nodes") {
      val root = new Root()
      val consNode = new OrNode(root, "Cons")
      root addChild consNode        
      
      val mergeNode = new AndNode(root, "merge")
      root addChild mergeNode
      val m1Node = new LeafNode(mergeNode, inputVar)
      mergeNode addChild m1Node
      val m2Node = new OrNode(mergeNode, "sameList")
      mergeNode.addChild(m2Node)
      val m21Node = new LeafNode(m2Node, inputVar)
      m2Node.addChild(m21Node)
              
      m2Node.setSolved(m21Node)
      mergeNode.setSolved(m1Node)
      
      m2Node.isSolved should be (true)
      m2Node.solvedNode should not be (null)
      mergeNode.isSolved should be (true)
      root.isSolved should be (true)
      root.solvedNode should not be (null)
      consNode.isSolved should be (false)
    }
   
  }

}
