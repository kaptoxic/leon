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
import org.scalatest.matchers.ShouldMatchers._

import java.io.{ BufferedWriter, FileWriter, File }

class SynthesizerTest extends FunSuite {

  import Scaffold._

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
    
    {
        
      import AndOrGraph._
      import StepGraph._
      import FragmentGraph._
      
      val operationTree = {
        val root = new RootStep()
        val consNode = new OrStep(root, "Cons")
        root addChild consNode        
        val consNode_1 = new LeafNode(consNode, inputVar)
        consNode_1 addChild consNode
        
        val mergeNode = new AndStep(root, "merge")
        root addChild mergeNode
        val m1Node = new LeafNode(mergeNode, inputVar)
        mergeNode addChild m1Node
        val m2Node = new OrStep(mergeNode, "sameList")
        mergeNode.addChild(m2Node)
        val m21Node = new LeafNode(m2Node, inputVar)
        m2Node.addChild(m21Node)
                
        consNode.setSolved(consNode_1)
        
        root
      }
      
      test("synthesizer.expand") {
        
        val exampleRoot = new RootFragment(operationTree, l1)

        def inverser(name: String)(frag: Expr) = (name, frag) match {
          case ("Cons", l1) => List( List(IntLiteral(1), nilExp) )
          case _ =>
            fail
        }
        
        val result = synthesizer.expand(exampleRoot, operationTree.getSolvedNode, inverser)
          
        result should not be (Nil)
        
        result match {
          case (el: AndFragment) :: Nil =>
            el.getChildren.size should be (2)
          case _ =>
            fail
        }
      }
      
//      test("synthesizer.propagate") {
//        
//        val exampleRoot = new RootFragment(operationTree, l1)
//
//        def inverser(name: String)(frag: Expr) = (name, frag) match {
//          case ("Cons", l1) => List( List(IntLiteral(1), nilExp) )
//          case _ =>
//            fail
//        }
//        
//        val result =
//          synthesizer.propagate(exampleRoot, (l1, l1), Map(l1 -> Map(l1 -> l1)), inverser)
//          
//        exampleRoot.solvedNode should not be (null)
//        exampleRoot.solvedNode match {
//          case x: AndNode =>
//          case _ => fail
//        }
//      }
            
    }
//    
//    
//    
//
//    object TestDecomposer extends (Expr => Map[Expr, Expr]) {
//      def apply(expr: Expr): Map[Expr, Expr] = expr match {
//        case `inputVar` =>
//          Map(nilExp -> inputVar)
//        case _ =>
//          throw new RuntimeException
//      }
//    }
  
//    test("given a single example") {
//      val examples: List[IO] = List( nil )
//    
//	    synthesizer.synthesize(inputVar.id, examples, functions, TestDecomposer) should
//	      be (Some(inputVar))	    
//    }
//    
//    test("given two examples") {
//      val synthesizer = new Synthesizer(codeGenEval)
//      val examples: List[IO] = List( nil, l1 )
//
//      synthesizer.synthesize(inputVar.id, examples, functions, TestDecomposer) should
//        be (Some(inputVar))     
//    }
//    
//    val l2 = (CaseClass(consClass, IntLiteral(2) :: nilExp :: Nil))
//    val l12 = (CaseClass(consClass, IntLiteral(1) :: l2 :: Nil))
//    
//    test("given 4 examples") {
//      val synthesizer = new Synthesizer(codeGenEval)
//      val examples: List[IO] = List( nil, l1, l2, l12 )
//
//      synthesizer.synthesize(inputVar.id, examples, functions, TestDecomposer) should
//        be (Some(inputVar))     
//    }
//    
//    val l21 = (CaseClass(consClass, IntLiteral(2) :: l1 :: Nil))
//
//    test("given 5 examples") {
//      val synthesizer = new Synthesizer(codeGenEval)
//      val examples: List[IO] = List( nil, l1, l2, l12, (l21, l12) )
//
//      synthesizer.synthesize(inputVar.id, examples, functions, TestDecomposer) should
//        be (Some(inputVar))     
//    }
  }

}
