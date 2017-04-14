package leon.test
package ioexamples

import leon._
import purescala._
import Expressions._
import Extractors._
import Definitions._
import Types._
import Common._

import leon.solvers._
import leon.solvers.z3._
import leon.solvers.Solver
import leon.synthesis.{ Synthesizer => _, _ }
import leon.synthesis.utils._
import leon.synthesis.ioexamples._
import leon.evaluators._

import leon.utils.logging._

import leon.test.condabd.util._

import org.scalatest._
import org.scalatest.Matchers._

import java.io.{ BufferedWriter, FileWriter, File }

class GraphStrategyTest extends FunSuite with Matchers with Inside with HasLogger {

  import Scaffold._
  import Constructors._
  import UtilCaseClass._

  import ExampleInputs._
  import Differencer.{ differences }
  import Util.w

  val ioExamplesTestcaseDir = "testcases/synthesis/io-examples/"

  val problems = forFile(ioExamplesTestcaseDir + "GraphStrategy.scala").toList

  test("check synthesis problems") {
    problems.size should be(2)
    
    problems.map(_._2.id.name) should contain ("problem")
  }

//  {
//    val (sctx, funDef, problem) = problems.find(_._2.id.name == "problem").get
//
//    implicit val program = sctx.program
//
//    val extraction = new ExamplesExtraction(sctx, sctx.program)
//    val examples = extraction.extract(problem)
//
//    test("extract examples") {
//      withClue(examples) {
//        examples.size should be(2)
//      }
//      
//      examples(1).toString should be (
//        "(List(" +
//          "(in,VertexInner(Cons[Edge](Edge(VertexInner(Cons[Edge](Edge(Transformed(D), 1), Nil[Edge]())), 13), Cons[Edge](Edge(VertexInner(Cons[Edge](Edge(Transformed(D), 3), Cons[Edge](Edge(VertexEnd(B), 6), Nil[Edge]()))), 7), Nil[Edge]()))))" +
//        ")," +
//          "(out,VertexInner(Cons[Edge](Edge(VertexInner(Cons[Edge](Edge(Transformed(D), 1), Nil[Edge]())), 13), Cons[Edge](Edge(VertexInner(Cons[Edge](Edge(Transformed(D), 3), Cons[Edge](Edge(Transformed(E), 6), Nil[Edge]()))), 7), Nil[Edge]())))))"
//      )
//      
//      program.definedFunctions.map(_.id.name) should contain ("v5t1")
//
//    }
//
//    test("get fragments") {
//
//      // get fragments
//      val ((inIds, outId), transformedExamples) = ExamplesExtraction.transformMappings(examples).get
//      info(s"inIds $inIds")
//      val unorderedFragments = Fragmenter.constructFragments(transformedExamples, inIds)
//
//      info("transformed examples: " + transformedExamples.mkString("\n"))
//      info("unordered fragments: " + unorderedFragments.mkString("\n"))
//      info("unordered fragments: " + unorderedFragments.map(CustomPrinter(_)).mkString("\n"))
////
////      val f1 :: f2 :: f3 :: Nil = unorderedFragments
////
////      val vars = problem.as.map(_.toVariable)
////      vars.size shouldBe 4
////
////      val elements1 :: elements2 :: elements3 :: Nil =
////        for (f <- unorderedFragments) yield ExprOps.collect[Expr]({
////          case cc @ CaseClass(ct, h :: t :: Nil) if ct.id.name == "Cons" => Set(h)
////          case _ => Set()
////        })(f)
////      info("elements2: " + elements2)
////
////      elements1 should have size 1
////      for (el2 <- elements2) {
////        val diffs = differences(elements1.head, el2, vars)
////        info(s"diffs between ${elements1.head} and $el2 are ${diffs.mkString("\n")}")
////      }
//
//    }
//  }
  
  {
    val (sctx, funDef, problem) = problems.find(_._2.id.name == "problemSimple").get

    implicit val program = sctx.program

    val extraction = new ExamplesExtraction(sctx, sctx.program)
    val examples = extraction.extract(problem)

    test("extract examples") {
      withClue(examples) {
        examples.size should be(2)
      }
      
    }

    test("get fragments") {

      // get fragments
      val ((inIds, outId), transformedExamples) = ExamplesExtraction.transformMappings(examples).get
      info(s"inIds $inIds")
      val unorderedFragments = Fragmenter.constructFragments(transformedExamples, inIds)

      info("transformed examples:\n" + transformedExamples.mkString("\n"))
      info("unordered fragments:\n" + unorderedFragments.mkString("\n"))
      
      val pathVar = FreshIdentifier("PATH", Untyped)
      
      val in1 :: in2 :: Nil  =
//        for (name <- "v5" :: "v5t1" :: "v5t" :: Nil) yield
//          program.definedFunctions.find(_.id.name == name).get.body.get
        transformedExamples.map(_._1.head)

      val f1 :: f2 :: Nil = unorderedFragments

      val inVar :: Nil = problem.as.map(_.toVariable)

    {
      val diffs = Differencer.differenceConstraintsRelaxed(in1, in2)
      diffs.size should be(1)
      
      val (a, b) = diffs.head
      
      val path1 = Util.mapOfSubexpressionsToPathFunctions(in1)(a)
      val path2 = Util.mapOfSubexpressionsToPathFunctions(in2)(b)
      
      // typing problem again, just compare strings
      path1(w).toString shouldBe path2(w).toString

    }

    }
  }

}