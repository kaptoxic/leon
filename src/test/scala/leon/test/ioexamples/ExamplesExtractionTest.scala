package leon.test
package ioexamples

import leon._
import leon.purescala.Definitions._
import leon.purescala.Trees._
import leon.purescala.TreeOps._
import leon.solvers.z3._
import leon.solvers.Solver
import leon.synthesis._
import leon.synthesis.utils._
import leon.synthesis.ioexamples._

import leon.test.condabd.util._

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers._

import java.io.{ BufferedWriter, FileWriter, File }

class ExamplesExtractionTest extends FunSuite {

  import Scaffold._

  val lesynthTestDir = "testcases/synthesis/"
  
  test("expected trees from passes should be returned") {
   
    val problems = forFile(lesynthTestDir + "ExamplesAsSpecifications.scala").toList
    problems.size should be (3)
    
    {
	    val (sctx, funDef, problem) = problems.head
	    
	    problem.as.size should be (1)
	    val in = problem.as.head.toVariable
	    problem.xs.size should be (1)
	    val out = problem.xs.head.toVariable
	    
	    problem.phi match {
	      case And(
	    		Implies(Equals(IntLiteral(-1), `in`), Equals(`out`, IntLiteral(-1))) ::
	    		Implies(Equals(IntLiteral(0), `in`), Equals(`out`, IntLiteral(0))) ::
	    		Implies(Equals(IntLiteral(42), `in`), Equals(`out`, IntLiteral(42))) :: Nil
	      ) =>
	      case _ => fail("Predicate was:\n" + problem.phi + "\nWe expected:\n" + And(
	    		Implies(Equals(IntLiteral(-1), in), Equals(out, IntLiteral(-1))) ::
	    		Implies(Equals(IntLiteral(0), in), Equals(out, IntLiteral(0))) ::
	    		Implies(Equals(IntLiteral(42), in), Equals(out, IntLiteral(42))) :: Nil
	      ))
	    }
    }
  }
  
  test("expected  mappings should be extracted") {
   
    val problems = forFile(lesynthTestDir + "ExamplesAsSpecifications.scala").toList
    problems.size should be (3)
    
    {
	    val (sctx, funDef, problem) = problems.head
	    
	    val mappings = ExamplesExtraction.extract(problem.phi)
	    
	    val ioExamples = ExamplesExtraction.transformMappings(mappings)
	    
	    ioExamples match {
	      case Some(((inId, outId), exampleList)) =>
	        inId should be (problem.as.head)
	        outId should be (problem.xs.head)
	        exampleList.size should be (3)
	      case _ => fail
	    }
    }
    
    {
      val (sctx, funDef, problem) = problems.find(_._2.id.name == "tail").get
      
      val program = sctx.program
      
//      val listClass = program.caseClassDef("List")
      val consClass = program.caseClassDef("Cons")
      val nilClass = program.caseClassDef("Nil")
      
      val nilExp = CaseClass(nilClass, Nil): Expr
      
      val mappings = ExamplesExtraction.extract(problem.phi)
      
      val ioExamples = ExamplesExtraction.transformMappings(mappings)
      
      ioExamples match {
        case Some(((inId, outId), exampleList)) =>
          inId should be (problem.as.head)
          outId should be (problem.xs.head)
          exampleList.size should be (3)
          
          exampleList should contain (
            (CaseClass(consClass, IntLiteral(0) :: nilExp :: Nil): Expr, nilExp)
          )
          exampleList should contain (
            (CaseClass(consClass, IntLiteral(0) ::
              (CaseClass(consClass, IntLiteral(1) :: nilExp :: Nil))
              :: Nil): Expr,
            CaseClass(consClass, IntLiteral(1) :: nilExp :: Nil): Expr)
          )
        case _ => fail
      }
    }
  }
    
//    {
//	    val (sctx, funDef, problem) = problems.toList(1)
//	    
//	    problem.as.size should be (1)
//	    val in = problem.as.head.toVariable
//	    problem.xs.size should be (1)
//	    val out = problem.xs.head.toVariable
//	    
//	    problem.phi match {
//	      case And(
//	    		Implies(Equals(IntLiteral(-1), `in`), Equals(`out`, IntLiteral(-1))) ::
//	    		Implies(Equals(IntLiteral(0), `in`), Equals(`out`, IntLiteral(0))) ::
//	    		Implies(Equals(IntLiteral(42), `in`), Equals(`out`, IntLiteral(42))) :: Nil
//	      ) =>
//	      case _ => fail("Predicate was:\n" + problem.phi + "\nWe expected:\n" + And(
//	    		Implies(Equals(IntLiteral(-1), in), Equals(out, IntLiteral(-1))) ::
//	    		Implies(Equals(IntLiteral(0), in), Equals(out, IntLiteral(0))) ::
//	    		Implies(Equals(IntLiteral(42), in), Equals(out, IntLiteral(42))) :: Nil
//	      ))
//	    }
//    }
//    
//    {
//	    val (sctx, funDef, problem) = problems.toList(2)
//	    
//	    problem.as.size should be (1)
//	    val in = problem.as.head.toVariable
//	    problem.xs.size should be (1)
//	    val out = problem.xs.head.toVariable
//	    
//	    problem.phi match {
//	      case And(
//	    		Implies(Equals(IntLiteral(-1), `in`), Equals(`out`, IntLiteral(-1))) ::
//	    		Implies(Equals(IntLiteral(0), `in`), Equals(`out`, IntLiteral(0))) ::
//	    		Implies(Equals(IntLiteral(42), `in`), Equals(`out`, IntLiteral(42))) :: Nil
//	      ) =>
//	      case _ => fail("Predicate was:\n" + problem.phi + "\nWe expected:\n" + And(
//	    		Implies(Equals(IntLiteral(-1), in), Equals(out, IntLiteral(-1))) ::
//	    		Implies(Equals(IntLiteral(0), in), Equals(out, IntLiteral(0))) ::
//	    		Implies(Equals(IntLiteral(42), in), Equals(out, IntLiteral(42))) :: Nil
//	      ))
//	    }
//    }
//  }
  
//
//  test("simple one-argument passes") {
//
//    for (
//      (sctx, f, p) <- forProgram(
//        """
//  	    import leon.Utils._
//
//				object TestObject {
//				  				  
//				  abstract class A
//				  case object B extends A
//				  case class C(a: Int, b: Int) extends A
//				  
//				  def testFun(in: A) = choose {
//				    (res: C) =>
//				      passes(
//					    Map[A, C](
//				          B -> C(1, 2),
//				          C(1, 0) -> C(1, 1)
//				    		),
//			        in, res
//					  )
//				  }
//  			}
//	    """)
//    ) {
//      val predicate = p.phi
//      val arguments = f.args.map(_.id).toSet
//      
//      val result = ExamplesExtraction.extract(predicate, arguments).toSet
//      
//      expect(2) {
//        result.size
//      }
//    }
//  }
//  
//  test("passes with tuple argument") {
//
//    for (
//      (sctx, f, p) <- forProgram(
//        """
//  	    import leon.Utils._
//
//		object TestObject {
//		  				  
//		  abstract class A
//		  case object B extends A
//		  case class C(a: Int, b: Int) extends A
//  	    
//  		  case class X(x: Int)
//		  
//		  def testFun(in: (A, A, X)) = choose {
//		    (res: C) =>
//		      passes(
//    		  Map[(A, A, X), C](
//		          (B, B, X(1)) -> C(1, 2),
//		          (C(1, 0), B, X(2)) -> C(1, 1)
//		    		),
//	        in, res
//			  )
//		  }
//  			}
//	    """)
//    ) {
//      val predicate = p.phi
//      val arguments = f.args.map(_.id).toSet
//      
//      val result = ExamplesExtraction.extract(predicate, arguments).toSet
//      expect(2) {
//        result.size
//      }
//      
//      for(exp <- "InputOutputExample(Map(in -> (B, B, X(1))),C(1, 2))" ::
//	    "InputOutputExample(Map(in -> (C(1, 0), B, X(2))),C(1, 1))" :: Nil)
//        result.map(_.toString).contains(exp)
//    }
//  }
//  
//  test("passes with tuple") {
//
//    for (
//      (sctx, f, p) <- forProgram(
//        """
//  	    import leon.Utils._
//
//		object TestObject {
//		  				  
//		  abstract class A
//		  case object B extends A
//		  case class C(a: Int, b: Int) extends A
//  	    
//  		  case class X(x: Int)
//		  
//		  def testFun(in1: A, in2: A, in3: X) = choose {
//		    (res: C) =>
//		      passes(
//    		  Map[(A, A, X), C](
//		          (B, B, X(1)) -> C(1, 2),
//		          (C(1, 0), B, X(2)) -> C(1, 1)
//		    		),
//	        (in1, in2, in3), res
//			  )
//		  }
//  			}
//	    """)
//    ) {
//      val predicate = p.phi
//      val arguments = f.args.map(_.id).toSet
//      
//      val result = ExamplesExtraction.extract(predicate, arguments).toSet
//      
//      expect(2) {
//        result.size
//      }
//      
//      for(exp <- "InputOutputExample(Map(in1 -> B, in2 -> B, in3 -> X(1)),C(1, 2))" ::
//	    "InputOutputExample(Map(in1 -> C(1, 0), in2 -> B, in3 -> X(2)),C(1, 1))" :: Nil)
//        result.map(_.toString).contains(exp)
//       
//    }
//  }

}
