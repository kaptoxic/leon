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

class PlanningWithErrorsTest extends FunSuite with Matchers with Inside with HasLogger {

  import Scaffold._
  import Constructors._
  import UtilCaseClass._

  import ExampleInputs._
  import Differencer.{ differences }
  import Util.w

  val ioExamplesTestcaseDir = "testcases/synthesis/io-examples/"

  val problems = forFile(ioExamplesTestcaseDir + "PlanningWithError.scala").toList

  test("check synthesis problem") {
    problems.size should be (3)
  }

  val (sctx, funDef, problem) = problems.find(_._2.id.name == "solveProblemDisks").get
  
 
  ignore("check synthesized size") {
    val funDef =
      sctx.program.definedFunctions.find(_.id.name == "solve").get
  
    print(
        ExprOps.formulaSize(funDef.fullBody)
    )
    ???
  }
 

  implicit val program = sctx.program

  test("extract examples") {
    
    val extraction = new ExamplesExtraction(sctx, sctx.program)
    val examples = extraction.extract(problem)
    withClue(examples) {
      examples.size should be (4)
    }

  }
  
  test("get fragments") {
    
    val extraction = new ExamplesExtraction(sctx, sctx.program)
    val examples = extraction.extract(problem)
    withClue(examples) {
      examples.size should be (4)
    }
    
    ///////////////////////
    
    val vars = problem.as.map(_.toVariable)
    
//    val hanoiClass = program.caseClassDef("Hanoi")
//    val vars = hanoiClass.fields.toList.map(_.toVariable)

    // get fragments
    val ((inIds, outId), transformedExamples) = ExamplesExtraction.transformMappings(examples).get
    info(s"inIds $inIds")
    val unorderedFragments = Fragmenter.constructFragments(transformedExamples, vars)

    info("transformed examples: " + transformedExamples.mkString("\n"))
    info("unordered fragments: " + unorderedFragments.mkString("\n"))
    info("unordered fragments: " + unorderedFragments.map(CustomPrinter(_)).mkString("\n"))
    
    val f1 :: f2 :: f3 :: f4 :: Nil = unorderedFragments
    
    
    val diffs12 =
    {
      // comparing l1 and nil should return nothing
      val diffs = differences(f1, f2, vars)
      fine(s"diffs between\n$f1\nand\n$f2\nare\n${diffs.map({
        case (k,v) => (k, v(w)) }).mkString("\n")}")
      diffs should not be ('empty)
      (f1, f2, diffs)
    }
    
    val diffs23 = 
    {
      // comparing l1 and nil should return nothing
      val diffs = differences(f2, f3, vars)
      fine(s"diffs between\n$f2\nand\n$f3\nare\n${diffs.map({
        case (k,v) => (k, v(w)) }).mkString("\n")}")
      diffs should not be ('empty)
      (f2, f3, diffs)
    }
    
    val fragments = unorderedFragments
    
    import Util.{ diffsToString, diffToString }
    
//    val allDiffs =
//      for ((f1, f2) <- fragments.take(2) zip fragments.tail.take(2)) yield {
//        val diffs = differences(f1, f2, vars)
//        fine(s"diffs between $f1 and $f2 are ${diffs.mkString("\n")}")
//        (f1, f2, diffs)
//      }

    val compatibles =
      for (
        (f11, f21, diffs1) <- diffs12 :: Nil;
        (f12, f22, diffs2) <- diffs23 :: Nil;
        if f11 != f12;
//        if f11.hashCode < f12.hashCode;
        diff1 <- diffs1;
        diff2 <- diffs2;
        _ = finer(s"Checking diffs: ${diff1: String}\nand\n${diff2: String}");
        merged <- Differencer.areCompatible(diff1, diff2);
        _ = finer(s"compatible!!")
      ) yield (f11 :: f21 :: f12 :: f22 :: Nil, merged)

    info("compatibles: " + compatibles.map(p => (p._1, (p._2._1, p._2._2(w)))).mkString("\n"))
    
//    val elements1 :: elements2 :: elements3 :: Nil = 
//      for (f <- unorderedFragments) yield
//        ExprOps.collect[Expr]({
//          case cc@CaseClass(ct, h :: t :: Nil) if ct.id.name == "Cons" => Set(h)
//          case _ => Set()
//        })(f)
//    fine("elements2: " + elements2)
//    
//    elements1 should have size 1
//    for (el2 <- elements2) {
//      val diffs = differences(elements1.head, el2, in :: Nil)
//      fine(s"diffs between ${elements1.head} and $el2 are ${diffs.map({
//        case (k,v) => (k, v(w)) }).mkString("\n")}")
//    }
    
  }

}