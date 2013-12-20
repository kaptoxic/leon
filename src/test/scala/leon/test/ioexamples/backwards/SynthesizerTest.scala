//package leon.test
//package ioexamples.backwards
//
//import leon.purescala.Definitions._
//import leon.purescala.Trees._
//import leon.purescala.TreeOps._
//import leon.solvers.z3._
//import leon.solvers.Solver
//import leon.synthesis.{ Synthesizer => _, _ }
//import leon.synthesis.utils._
//import leon.synthesis.ioexamples.backwards._
//import leon.evaluators._
//
//import leon.test.condabd.util._
//
//import org.scalatest.FunSuite
//import org.scalatest.matchers.ShouldMatchers._
//
//import java.io.{ BufferedWriter, FileWriter, File }
//
//class SynthesizerTest extends FunSuite {
//
//  import Scaffold._
//
//  val testDir = "testcases/ioexamples/"
//    
//  val problems = forFile(testDir + "MergeSort_Sort.scala").toList
//  problems.size should be (1)
//    
//  type IO = (Expr, Expr)
//  // may come in handy
//  implicit def exprToPair(expr: Expr) = (expr, expr)
//    
//  val (sctx, funDef, problem) = problems.head
//  
//  val program = sctx.program
//  val inputVar = funDef.args.head.toVariable
//  val codeGenEval = new CodeGenEvaluator(sctx.context, sctx.program)
//  
//  val consClass = program.caseClassDef("Cons")
//  val nilClass = program.caseClassDef("Nil")
//  val nilExp = CaseClass(nilClass, Nil): Expr
//  
//  val functions = program.definedFunctions.groupBy( f => f.returnType)
//  
//  val mergeFun = program.definedFunctions.find(_.id.name == "merge").get
//  val sortFun = program.definedFunctions.find(_.id.name == "sort").get
//  
//  val nil = nilExp
//  val l1 = (CaseClass(consClass, IntLiteral(1) :: nilExp :: Nil))
//  val l2 = (CaseClass(consClass, IntLiteral(2) :: nilExp :: Nil))
//  val l3 = (CaseClass(consClass, IntLiteral(3) :: nilExp :: Nil))
//  val l12 = (CaseClass(consClass, IntLiteral(1) :: l2 :: Nil))
//  val l21 = (CaseClass(consClass, IntLiteral(2) :: l1 :: Nil))
//  val l13 = (CaseClass(consClass, IntLiteral(1) :: l3 :: Nil))
//  val l23 = (CaseClass(consClass, IntLiteral(2) :: l3 :: Nil))
//  val l123 = (CaseClass(consClass, IntLiteral(1) :: l23 :: Nil))
//  val l213 = (CaseClass(consClass, IntLiteral(2) :: l13 :: Nil))
//  
//  val l21tol1 = (CaseClass(consClass,
//    CaseClassSelector(consClass, inputVar, consClass.fields.find(_.id.name == "head").get.id) :: nilExp :: Nil))
//  val l21tol2 = 
//    CaseClassSelector(consClass, inputVar, consClass.fields.find(_.id.name == "tail").get.id)
//  val l213tol21 =
////    (CaseClass(consClass,
//      nilExp
////    CaseClassSelector(consClass, inputVar, consClass.fields.find(_.id.name == "tail").get.id) :: nilExp :: Nil))
//  val l213tol3 = 
//    CaseClassSelector(consClass, inputVar, consClass.fields.find(_.id.name == "tail").get.id)
//  
//  val synthesizer = new Synthesizer(codeGenEval)
//
//  import AndOrGraph._
//  import StepGraph._
//  import FragmentGraph._
//  
////  {
////    test("synthesizer.expand or-node") {
////    
////      val operationTree = {
////        val root = new RootStep()
////        val consNode = new OrStep(root, "Cons")
////        root addChild consNode        
////        
////        val mergeNode = new AndStep(root, "merge")
////        root addChild mergeNode
////        val m1Node = new OrStep(mergeNode, "sameList")
////        mergeNode addChild m1Node
////        val m2Node = new OrStep(mergeNode, "sameList")
////        mergeNode.addChild(m2Node)
////                
////        root.setSolved(consNode)
////        
////        root
////      }
////      
////      val exampleRoot = new RootFragment(operationTree, l1)
////
////      def inverser(name: String)(frag: Expr) = (name, frag) match {
////        case ("Cons", l1) => List( List(IntLiteral(1)) )
////        case _ =>
////          fail
////      }
////      
////      val result = synthesizer.expand(exampleRoot, operationTree.getSolvedNode, inverser)
////        
////      result should not be (Nil)
////      
////      result match {
////        case (el: OrFragment) :: Nil =>
////          el.getChildren.size should be (0)
////        case _ =>
////          fail
////      }
////    }
////
////    test("synthesizer.expand and-node") {
////    
////      val operationTree = {
////        val root = new RootStep()
////        val consNode = new OrStep(root, "Cons")
////        root addChild consNode        
////
////        val mergeNode = new AndStep(root, "merge")
////        root addChild mergeNode
////        val m1Node = new OrStep(mergeNode, inputVar)
////        mergeNode addChild m1Node
////        val m2Node = new OrStep(mergeNode, inputVar)
////        mergeNode.addChild(m2Node)
////                
////        mergeNode.setSolved(m1Node)
////        mergeNode.setSolved(m2Node)
////        
////        root
////      }
////      
////      val exampleRoot = new RootFragment(operationTree, l1)
////
////      def inverser(name: String)(frag: Expr) = (name, frag) match {
////        case ("merge", l1) => List( List(IntLiteral(1), nilExp) )
////        case _ =>
////          fail
////      }
////      
////      val result = synthesizer.expand(exampleRoot, operationTree.getSolvedNode, inverser)
////        
////      result should not be (Nil)
////      
////      result match {
////        case (el: AndFragment) :: Nil =>
////          el.getChildren.size should be (2)
////        case _ =>
////          fail
////      }
////    }
////    
////    test("synthesizer.propagate single node") {
////    
////      val root = new RootStep()
//////        val consNode = new OrStep(root, "Cons")
//////        root addChild consNode
////      val inputNode = new OrStep(root, "Cons", inputVar)
////      root addChild inputNode
////      
////      val mergeNode = new AndStep(root, "merge")
////      root addChild mergeNode
////      val m1Node = new OrStep(mergeNode, inputVar)
////      mergeNode addChild m1Node
////      val m2Node = new OrStep(mergeNode, "sameList")
////      mergeNode.addChild(m2Node)
////
////      val operationTree = root
////              
////      root.setSolved(inputNode)
////      
////      assert(root.isSolved)
////      assert(inputNode.isSolved)
////      
////      val exampleRoot = new RootFragment(operationTree, l1)
////
////      def inverser(name: String)(frag: Expr) = (name, frag) match {
////        case ("Cons", l1) => List( List( l1 ) )
////        case ("merge", l1) => List( List( l1, nilExp ) )
////        case _ =>
////          fail
////      }
////      
////      val result =
////        synthesizer.propagate(exampleRoot, Map(l1 -> Map(l1 -> inputVar)), inverser)
////        
////      exampleRoot.isSolved should be (true)
////      exampleRoot.getSolvedNode match {
////        case of: OrFragment =>
////          of.fragment should be (l1)
////        case _ => fail
////      }
////
////      withClue("Got map: " + exampleRoot.getMap.toString) {
////        exampleRoot.getMap should not be ('empty)
////      }
////    }
////    
////    test("synthesizer.propagate single root") {
////    
////      val root = new RootStep()
////      root.solution = inputVar      
////              
////      assert(root.isSolved)
////      
////      val exampleRoot = new RootFragment(root, l1)
////
////      def inverser(name: String)(frag: Expr) = (name, frag) match {
////        case ("root", l1) => List( List( l1 ) )
////        case _ =>
////          fail
////      }
////      
////      val result =
////        synthesizer.propagate(exampleRoot, Map(l1 -> Map(l1 -> inputVar)), inverser)
////        
////      exampleRoot.isSolved should be (true)
////      exampleRoot.getSolvedNode should be (exampleRoot)
////    }
////    
////    test("synthesizer.propagate mergesort example") {
////      
////      val makeFirstList = (CaseClass(consClass,
////        CaseClassSelector(consClass, inputVar, consClass.fields.find(_.id.name == "head").get.id) :: nilExp :: Nil))
////      val secondList = 
////        CaseClassSelector(consClass, inputVar, consClass.fields.find(_.id.name == "tail").get.id)
////    
////      val root = new RootStep()
////      val consNode = new AndStep(root, "Cons")
////      root addChild consNode
////      
////      val mergeNode = new AndStep(root, "merge")
////      root addChild mergeNode
////      val m1Node = new OrStep(mergeNode, secondList)
////      mergeNode addChild m1Node
////      val m2Node = new OrStep(mergeNode, makeFirstList)
////      mergeNode.addChild(m2Node)
////            
////      mergeNode.setSolved(m2Node)
////      mergeNode.setSolved(m1Node)
////      
////      val example = (l21, l12)
////      
////      val exampleRoot = new RootFragment(root, example)
////
////      def inverser(name: String)(frag: Expr) = (name, frag) match {
////        case ("Cons", `l1`) => List( List(IntLiteral(1), nilExp) )
////        case ("Cons", `l12`) => List( List(IntLiteral(1), l2) )
////        case ("merge", `l21`) => List( List(l2, l1) )
////        case ("merge", `l12`) => List( List(l1, l2) )
////        case _ =>
////          fail
////      }
////      
////      val subexpressions: Map[Expr, Map[Expr, Expr]] = Map(
////        l21 -> Map(l21 -> inputVar, l1 -> secondList, l2 -> makeFirstList)
////      )
////      
////      val result =
////        synthesizer.propagate(exampleRoot, subexpressions, inverser)
////        
////      exampleRoot.isSolved should be (true)
////      exampleRoot.getSolvedNode match {
////        case af: AndFragment =>
////          af.getChildren match {
////            case (of1: OrFragment) :: (of2: OrFragment) :: Nil =>
////              of1.fragment should be (l1)
////              of2.fragment should be (l2)
////            case _ =>
////              fail
////          }
////        case _ => fail
////      }
////
////      exampleRoot.getMap should not be ('empty)
////    }
////          
////  }
////    
////    test("synthesizer.explore single root") {
////    
////      val root = new RootStep()
////      
////      val exampleRoot = new RootFragment(root, (l1, l2))
////
////      def inverser(name: String)(frag: Expr) = (name, frag) match {
////        case ("magic", l2) => List( List( l1 ) )
////        case _ =>
////          fail
////      }
////      
////      def functions(e: Expr) = e match {
////        case `l2` => List("magic")
//////        case `l1` => List()
////      }
////      
////      synthesizer.explore(exampleRoot, functions,
////        Map(l1 -> Map(l1 -> inputVar)), inverser)
////        
////      exampleRoot.isSolved should be (true)
////    }
////    
////    test("synthesizer.explore mergesort example") {
////    
////      val root = new RootStep()
//////      val consNode = new AndStep(root, "Cons")
//////      root addChild consNode
//////      
//////      val mergeNode = new AndStep(root, "merge")
//////      root addChild mergeNode
//////      val m1Node = new OrStep(mergeNode, secondList)
//////      mergeNode addChild m1Node
//////      val m2Node = new OrStep(mergeNode, makeFirstList)
//////      mergeNode.addChild(m2Node)
//////            
//////      mergeNode.setSolved(m2Node)
//////      mergeNode.setSolved(m1Node)
////      
////      val example = (l21, l12)
////      
////      val exampleRoot = new RootFragment(root, example)
////
////      def inverser(name: String)(frag: Expr) = (name, frag) match {
////        case ("Cons", `l1`) => List( List(IntLiteral(1), nilExp) )
////        case ("Cons", `l12`) => List( List(IntLiteral(1), l2) )
////        case ("merge", `l21`) => List( List(l2, l1) )
////        case ("merge", `l12`) => List( List(l1, l2) )
////        case _ =>
////          fail
////      }
////      
////      val subexpressions: Map[Expr, Map[Expr, Expr]] = Map(
////        l21 -> Map(l21 -> inputVar, l1 -> l21tol1, l2 -> l21tol2)
////      )
////      
////      def functions(e: Expr) = e match {
////        case `l12` => List("Cons", "merge")
////        case `l2` => List()
////        case `l1` => List()
////        case IntLiteral(1) => List()
////        case IntLiteral(0) => List()
////      }
////      
////      val result =
////        synthesizer.explore(exampleRoot, functions, subexpressions, inverser)
////        
////      exampleRoot.isSolved should be (true)
////      exampleRoot.getSolvedNode match {
////        case af: AndFragment =>
////          af.step.stepFun should be ("merge")
////          af.getChildren match {
////            case (of1: OrFragment) :: (of2: OrFragment) :: Nil =>
////              of1.fragment should be (l1)
////              of2.fragment should be (l2)
////              of1.isSolved should be (true)
////              of2.isSolved should be (true)
////            case _ =>
////              fail
////          }
////        case _ => fail
////      }
////
////      exampleRoot.getMap should not be ('empty)
////    }
////
////    test("synthesizer.explore mergesort example, queue") {
////    
////      val root = new RootStep()
//////      val consNode = new AndStep(root, "Cons")
//////      root addChild consNode
//////      
//////      val mergeNode = new AndStep(root, "merge")
//////      root addChild mergeNode
//////      val m1Node = new OrStep(mergeNode, secondList)
//////      mergeNode addChild m1Node
//////      val m2Node = new OrStep(mergeNode, makeFirstList)
//////      mergeNode.addChild(m2Node)
//////            
//////      mergeNode.setSolved(m2Node)
//////      mergeNode.setSolved(m1Node)
////      
////      val example = (l21, l12)
////      
////      val exampleRoot = new RootFragment(root, example)
////
////      def inverser(name: String)(frag: Expr) = (name, frag) match {
////        case ("Cons", `l1`) => List( List(IntLiteral(1), nilExp) )
////        case ("Cons", `l12`) => List( List(IntLiteral(1), l2) )
////        case ("merge", `l21`) => List( List(l2, l1) )
////        case ("merge", `l12`) => List( List(l1, l2) )
////        case _ =>
////          fail
////      }
////      
////      val subexpressions: Map[Expr, Map[Expr, Expr]] = Map(
////        l21 -> Map(l21 -> inputVar, l1 -> l21tol1, l2 -> l21tol2)
////      )
////      
////      def functions(e: Expr) = e match {
////        case `l12` => List("Cons", "merge")
////        case `l2` => List()
////        case `l1` => List()
////        case IntLiteral(1) => List()
////        case IntLiteral(0) => List()
////      }
////      
////      val result =
////        synthesizer.explore(exampleRoot, functions, subexpressions, inverser)
////        
////      result.size should be (3)
////    }
//          
//    {
//      def inverser(name: String)(frag: Expr) = (name, frag) match {
////        case ("Cons", `l1`) => List( List(IntLiteral(1), nilExp) )
////        case ("Cons", `l12`) => List( List(IntLiteral(1), l2) )
////        case ("merge", `l21`) => List( List(l2, l1) )
////        case ("merge", `l12`) => List( List(l1, l2) )
//        case _ =>
//          fail
//      }
//      
//      val identities: Map[Expr, Map[Expr, Expr]] = List(nilExp, l1, l2, l3, l12, l23, l123, l213)
//        .map(e => (e, Map(e -> inputVar))).toMap
//      val subexpressions: Map[Expr, Map[Expr, Expr]] = (Map(
//        l21 -> Map(l21 -> inputVar, l1 -> l21tol1, l2 -> l21tol2)
//      ): Map[Expr, Map[Expr, Expr]]) ++ identities
//      
//      def functions(e: Expr) = e match {
//        case `l123` => List("merge")
//        case `l12` | `l23` => List("merge")
////        case `l12` | `l23` => List("Cons", "merge")
//        case `l1` | `l2` | `l3` => List()
//        case IntLiteral(_) => List()
//        case _ => List()
//      }
//      
//      def cst(op: String, exprs: List[Expr]): Expr = (op, exprs) match {
//        case _ => inputVar
//      }
//    
////      test("given a single example") {
////        val examples: List[IO] = List( nil )
////      
////        synthesizer.synthesize(inputVar.id, examples, functions, subexpressions, cst, inverser)
////  	      be (Some(inputVar))	    
////      }
////      
////      test("given two examples") {
////        val synthesizer = new Synthesizer(codeGenEval)
////        val examples: List[IO] = List( nil, l1 )
////  
////        synthesizer.synthesize(inputVar.id, examples, functions, subexpressions, cst, inverser)
////          be (Some(inputVar))     
////      }
////      
////      test("given 4 examples") {
////        val synthesizer = new Synthesizer(codeGenEval)
////        val examples: List[IO] = List( nil, l1, l2, l12 )
////  
////        synthesizer.synthesize(inputVar.id, examples, functions, subexpressions, cst, inverser)
////          be (Some(inputVar))       
////      }
////      
////      test("given 5 examples") {
////        val synthesizer = new Synthesizer(codeGenEval)
////        val examples: List[IO] = List( nil, l1, l2, l12, (l21, l12) )
////        
////        def partition(exs: List[IO]): (Expr, List[IO], List[IO]) =
////          (BooleanLiteral(false), List(nil, l1, l2, l12), List((l21, l12)))
////        
////        def inverser(name: String)(frag: Expr) = (name, frag) match {
////          case ("Cons", `l1`) => List( List(IntLiteral(1), nilExp) )
////          case ("Cons", `l12`) => List( List(IntLiteral(1), l2) )
//////          case ("merge", `l21`) => List( List(l2, l1) )
////          case ("merge", `l12`) => List( List(l1, l2) )
////          case _ =>
////            fail
////        }
////        
////        def cst(op: String, exprs: List[Expr]): Expr = (op, exprs) match {
////          case ("merge", _) =>
////            FunctionInvocation(mergeFun, exprs)
////          case ("root", sol :: Nil) =>
////            sol
////          case _ => fail("no op for: " + (op, exprs) )
////        }
////  
////        synthesizer.synthesize(inputVar.id, examples, functions, subexpressions, cst, inverser, partition) should
////          be (Some(FunctionInvocation(mergeFun, l21tol1 :: l21tol2 :: Nil)))    
////      }
//      
//    test("given 5 examples") {
////      val examples: List[IO] = List( nil, l1, l2, l3, l12, (l21, l12), (l213, l123) )
////      
////      def partition(exs: List[IO]): (Expr, List[IO], List[IO]) =
////        (BooleanLiteral(false), List(nil, l1, l2, l12), List((l21, l12)))
//      
//      val examples: List[IO] = List( (l21, l12), (l213, l123) )
//      
//      def partition(exs: List[IO]): (Expr, List[IO], List[IO]) =
//        null
//      
//      def inverser(name: String)(frag: Expr) = (name, frag) match {
////        case ("Cons", `l1`) => List( List(IntLiteral(1), nilExp) )
////        case ("Cons", `l12`) => List( List(IntLiteral(1), l2) )
////        case ("Cons", `l23`) => List( List(IntLiteral(2), l3) )
////        case ("Cons", `l123`) => List( List(IntLiteral(1), l1) )
//        case ("merge", `l12`) => List( List(l1, l2) )
//        case ("merge", `l123`) => List( List(l12, l3) )
//        case ("merge", `l23`) => List( List(l2, l3) )
//        case ("rec", _) => Nil
//        case _ =>
//          fail("Cannot process: " + name + ", " + frag)
//      }
//
//      val recCall = sortFun.id.toVariable
//      
//      val subexpressions: Map[Expr, Map[Expr, Expr]] = (Map(
//        l21 -> Map(l21 -> inputVar, l1 -> l21tol1, l2 -> l21tol2),
//        l213 -> Map(l213 -> inputVar, l21 -> l213tol21, l3 -> l213tol3)
//      ): Map[Expr, Map[Expr, Expr]]) //++ identities
//
//      def cst(op: String, exprs: List[Expr]): Expr = (op, exprs) match {
//        case ("merge", _) =>
//          FunctionInvocation(mergeFun, exprs)
//        case ("root", sol :: Nil) =>
//          sol
//        case ("rec", exprs) =>
//          FunctionInvocation(sortFun, exprs)
//        case _ => fail("no op for: " + (op, exprs) )
//      }
//
//      synthesizer.synthesize(inputVar.id, examples, functions, subexpressions, cst, inverser, partition) should
//        be (Some(FunctionInvocation(mergeFun, FunctionInvocation(sortFun, l213tol21 :: Nil) :: l213tol3 :: Nil)))    
//    }
//
//  }
//
//}
