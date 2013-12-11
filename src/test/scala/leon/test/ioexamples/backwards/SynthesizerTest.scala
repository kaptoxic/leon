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
  val l2 = (CaseClass(consClass, IntLiteral(2) :: nilExp :: Nil))
  val l12 = (CaseClass(consClass, IntLiteral(1) :: l2 :: Nil))
  val l21 = (CaseClass(consClass, IntLiteral(2) :: l1 :: Nil))
  
  val synthesizer = new Synthesizer(codeGenEval)
  
  {
    import AndOrGraph._
    import StepGraph._
    import FragmentGraph._
    
    test("synthesizer.expand or-node") {
    
      val operationTree = {
        val root = new RootStep()
        val consNode = new OrStep(root, "Cons")
        root addChild consNode        
        
        val mergeNode = new AndStep(root, "merge")
        root addChild mergeNode
        val m1Node = new OrStep(mergeNode, "sameList")
        mergeNode addChild m1Node
        val m2Node = new OrStep(mergeNode, "sameList")
        mergeNode.addChild(m2Node)
                
        root.setSolved(consNode)
        
        root
      }
      
      val exampleRoot = new RootFragment(operationTree, l1)

      def inverser(name: String)(frag: Expr) = (name, frag) match {
        case ("Cons", l1) => List( List(IntLiteral(1)) )
        case _ =>
          fail
      }
      
      val result = synthesizer.expand(exampleRoot, operationTree.getSolvedNode, inverser)
        
      result should not be (Nil)
      
      result match {
        case (el: OrFragment) :: Nil =>
          el.getChildren.size should be (0)
        case _ =>
          fail
      }
    }

    test("synthesizer.expand and-node") {
    
      val operationTree = {
        val root = new RootStep()
        val consNode = new OrStep(root, "Cons")
        root addChild consNode        

        val mergeNode = new AndStep(root, "merge")
        root addChild mergeNode
        val m1Node = new OrStep(mergeNode, inputVar)
        mergeNode addChild m1Node
        val m2Node = new OrStep(mergeNode, inputVar)
        mergeNode.addChild(m2Node)
                
        mergeNode.setSolved(m1Node)
        mergeNode.setSolved(m2Node)
        
        root
      }
      
      val exampleRoot = new RootFragment(operationTree, l1)

      def inverser(name: String)(frag: Expr) = (name, frag) match {
        case ("merge", l1) => List( List(IntLiteral(1), nilExp) )
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
    
    test("synthesizer.propagate single node") {
    
      val root = new RootStep()
//        val consNode = new OrStep(root, "Cons")
//        root addChild consNode
      val inputNode = new OrStep(root, "Cons", inputVar)
      root addChild inputNode
      
      val mergeNode = new AndStep(root, "merge")
      root addChild mergeNode
      val m1Node = new OrStep(mergeNode, inputVar)
      mergeNode addChild m1Node
      val m2Node = new OrStep(mergeNode, "sameList")
      mergeNode.addChild(m2Node)

      val operationTree = root
              
      root.setSolved(inputNode)
      
      assert(root.isSolved)
      assert(inputNode.isSolved)
      
      val exampleRoot = new RootFragment(operationTree, l1)

      def inverser(name: String)(frag: Expr) = (name, frag) match {
        case ("Cons", l1) => List( List( l1 ) )
        case ("merge", l1) => List( List( l1, nilExp ) )
        case _ =>
          fail
      }
      
      val result =
        synthesizer.propagate(exampleRoot, Map(l1 -> Map(l1 -> inputVar)), inverser)
        
      exampleRoot.isSolved should be (true)
      exampleRoot.getSolvedNode match {
        case of: OrFragment =>
          of.fragment should be (l1)
        case _ => fail
      }

      withClue("Got map: " + exampleRoot.getMap.toString) {
        exampleRoot.getMap should not be ('empty)
      }
    }
    
    test("synthesizer.propagate single root") {
    
      val root = new RootStep()
      root.solution = inputVar      
              
      assert(root.isSolved)
      
      val exampleRoot = new RootFragment(root, l1)

      def inverser(name: String)(frag: Expr) = (name, frag) match {
        case ("root", l1) => List( List( l1 ) )
        case _ =>
          fail
      }
      
      val result =
        synthesizer.propagate(exampleRoot, Map(l1 -> Map(l1 -> inputVar)), inverser)
        
      exampleRoot.isSolved should be (true)
      exampleRoot.getSolvedNode should be (exampleRoot)
    }
    
    test("synthesizer.propagate mergesort example") {
      
      val makeFirstList = (CaseClass(consClass,
        CaseClassSelector(consClass, inputVar, consClass.fields.find(_.id.name == "head").get.id) :: nilExp :: Nil))
      val secondList = 
        CaseClassSelector(consClass, inputVar, consClass.fields.find(_.id.name == "tail").get.id)
    
      val root = new RootStep()
      val consNode = new AndStep(root, "Cons")
      root addChild consNode
      
      val mergeNode = new AndStep(root, "merge")
      root addChild mergeNode
      val m1Node = new OrStep(mergeNode, secondList)
      mergeNode addChild m1Node
      val m2Node = new OrStep(mergeNode, makeFirstList)
      mergeNode.addChild(m2Node)
            
      mergeNode.setSolved(m2Node)
      mergeNode.setSolved(m1Node)
      
      val example = (l21, l12)
      
      val exampleRoot = new RootFragment(root, example)

      def inverser(name: String)(frag: Expr) = (name, frag) match {
        case ("Cons", `l1`) => List( List(IntLiteral(1), nilExp) )
        case ("Cons", `l12`) => List( List(IntLiteral(1), l2) )
        case ("merge", `l21`) => List( List(l2, l1) )
        case ("merge", `l12`) => List( List(l1, l2) )
        case _ =>
          fail
      }
      
      val subexpressions: Map[Expr, Map[Expr, Expr]] = Map(
        l21 -> Map(l21 -> inputVar, l1 -> secondList, l2 -> makeFirstList)
      )
      
      val result =
        synthesizer.propagate(exampleRoot, subexpressions, inverser)
        
      exampleRoot.isSolved should be (true)
      exampleRoot.getSolvedNode match {
        case af: AndFragment =>
          af.getChildren match {
            case (of1: OrFragment) :: (of2: OrFragment) :: Nil =>
              of1.fragment should be (l1)
              of2.fragment should be (l2)
            case _ =>
              fail
          }
        case _ => fail
      }

      exampleRoot.getMap should not be ('empty)
    }
          
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
