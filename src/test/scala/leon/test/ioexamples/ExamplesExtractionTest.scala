package leon.test
package ioexamples

import leon._
import purescala._
import Expressions._
import Definitions._
import Types._
import leon.solvers.z3._
import leon.solvers.Solver
import leon.synthesis._
import leon.synthesis.utils._
import leon.synthesis.ioexamples._

import leon.test.condabd.util._

import org.scalatest._
import org.scalatest.Matchers._

import java.io.{ BufferedWriter, FileWriter, File }

class ExamplesExtractionTest extends FunSpec with Inside {

  import Scaffold._

  val ioExamplesTestcaseDir = "testcases/synthesis/io-examples/"

  describe("parsing input-output examples specs") {

    describe("ExamplesAsSpecifications testcase") {

      val problems = forFile(ioExamplesTestcaseDir + "ExamplesAsSpecifications.scala").toList

      describe("expected trees from passes should be returned") {

        it("should parse all problems") {
          problems.size should be(3)
        }
      }

      describe("should parse passes with map") {
        val (sctx, funDef, problem) = problems.head

        it("should correctly construct identity") {

          problem.as.size should be(1)
          val in = problem.as.head
          problem.xs.size should be(1)
          val out = problem.xs.head

          withClue(problem) {
            val extraction = new ExamplesExtraction(sctx, sctx.program)
            val examples = extraction.extract(problem)

            withClue(examples) {
              examples should not be ('empty)

              examples should contain allOf (
                ((in, IntLiteral(-1)) :: Nil, (out, IntLiteral(-1))),
                ((in, IntLiteral(0)) :: Nil, (out, IntLiteral(0))),
                ((in, IntLiteral(42)) :: Nil, (out, IntLiteral(42))))
            }
          }
        }
      }

      describe("should parse problems with match passes") {

        it("should correctly construct tail") {
          val (sctx, funDef, problem) = problems.find(_._2.id.name == "tail").get

          val program = sctx.program

          val consClass = program.caseClassDef("Cons").typed
          val nilClass = program.caseClassDef("Nil").typed
          val nilExp = CaseClass(nilClass, Nil): Expr

          withClue(problem) {

            val extraction = new ExamplesExtraction(sctx, sctx.program)
            val examples = extraction.extract(problem)

            for (ioExample <- examples) {
              inside(ioExample) {
                case (in, out) =>
                  inside(in) {
                    case (inId, inExpr) :: Nil => inId.name shouldBe "in"
                  }
                  inside(out) {
                    case (outId, _) => outId.name shouldBe "out"
                  }
              }
            }

            examples.size should be(3)
            examples.map { ex =>
              (ex._1.head._2, ex._2._2)
            } should contain allOf (
              (CaseClass(consClass, IntLiteral(0) :: nilExp :: Nil): Expr, nilExp),
              (CaseClass(consClass, IntLiteral(0) ::
                (CaseClass(consClass, IntLiteral(1) :: nilExp :: Nil))
                :: Nil): Expr,
                CaseClass(consClass, IntLiteral(1) :: nilExp :: Nil): Expr))
          }
        }
      }
    }

    describe("editor testcase") {

      val problems = forFile(ioExamplesTestcaseDir + "Editor.scala").toList

      describe("expected trees from passes should be returned") {

        it("should parse all problems") {
          problems.size should be (1)
        }

      }

      describe("should parse passes with map") {

        val (sctx, funDef, problem) = problems.head

        it("should correctly construct identity") {

          problem.as.size should be(1)
          val in = problem.as.head
          problem.xs.size should be(1)
          val out = problem.xs.head

          withClue(problem) {
            val extraction = new ExamplesExtraction(sctx, sctx.program)
            val examples = extraction.extract(problem)

            withClue(examples) {
              examples should not be ('empty)

              val program = sctx.program
    
              val consClass = program.caseClassDef("Cons").typed
              val nilClass = program.caseClassDef("Nil").typed
              val nilExp = CaseClass(nilClass, Nil): Expr

              examples should contain
                (
                  CaseClass(consClass, IntLiteral(0) ::
                    CaseClass(consClass, IntLiteral(2) :: nilExp :: Nil) :: Nil)
                ,
                  CaseClass(consClass, IntLiteral(2) :: nilExp :: Nil)
                )
              
            }
          }
        }
      }

    }

  }

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
