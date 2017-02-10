package leon.test
package ioexamples

import leon._
import purescala._
import Expressions._
import Definitions._
import Types._
import Common._

import leon.solvers.z3._
import leon.solvers.Solver
import leon.synthesis._
import leon.synthesis.utils._
import leon.synthesis.ioexamples._

import leon.utils.logging._

import leon.test.condabd.util._

import org.scalatest._
import org.scalatest.Matchers._

import java.io.{ BufferedWriter, FileWriter, File }

class MergeSortTest extends FunSuite with Matchers with Inside with HasLogger {

  import Scaffold._
  import Constructors._

  val ioExamplesTestcaseDir = "testcases/synthesis/io-examples/"
 
  def caseClassDef(name: String)(implicit pgm: Program): CaseClassDef = {
    pgm.lookupAll(name).collect {
      case ccd: CaseClassDef => ccd
    }.headOption.getOrElse {
      fail(s"Failed to lookup case class '$name' in program")
    }
  } 
  
  def cc(name: String)(args: Expr*)(implicit pgm: Program): Expr = {
    val cct = caseClassDef(name).typed(Seq())
    CaseClass(cct, args.toSeq)
  }

  test("playing") {
    
    val problems = forFile(ioExamplesTestcaseDir + "MergeSortMerge.scala").toList
    problems.size should be (1)
    
    val (sctx, funDef, problem) = problems.head
    
    implicit val program = sctx.program
    
    val consClass = program.caseClassDef("Cons").typed
    val nilClass = program.caseClassDef("Nil").typed
    val nilExp = CaseClass(nilClass, Nil): Expr
    
    def t(expr: Expr) = {
      caseClassSelector(consClass, expr, consClass.fields.find(_.id.name == "tail").get.id)
//      CaseClassSelector(consClass, expr, consClass.fields.find(_.id.name == "tail").get.id)
    }
    
    val l = Variable(FreshIdentifier("l", consClass))
    
    val extraction = new ExamplesExtraction(sctx, sctx.program)
    val examples = extraction.extract(problem)
    withClue(examples) {
      examples.size should be (5)
    }
    
    import Util._
    import ExampleInputs._
    
    import Differencer.{ differences }
    
    // get fragments
    val ((inIds, outId), transformedExamples) = extraction.transformMappings(examples).get
    info(s"inIds $inIds")
    val unorderedFragments = Fragmenter.constructFragments(transformedExamples, inIds)
    
    info(transformedExamples.mkString("\n"))
    info(unorderedFragments.mkString("\n"))
    info(unorderedFragments.map(CustomPrinter(_)).mkString("\n"))
    
    val l1 :: l2 :: Nil = problem.as.map(_.toVariable)
    
//    [h(l1),h(l2)t(l1)]
//    [h(l2)l1]
//    l1
//    [h(l2),h(l1),ht(l2)t(l1)]
//    nil
    val (e4 :: e3 :: e2 :: e5 :: e1 :: Nil) = unorderedFragments
    val fragments = e1 :: e2 :: e3 :: e4 :: e5 :: Nil
    
//    println(mapOfSubexpressions(e3))
    
    
    {
      // comparing l1 and nil should return nothing
      val diffs = differences(e1, e2, l1 :: l2 :: Nil)
      diffs should be ('empty)
    }
    
    {
      val diffs = differences(e2, e3, l1 :: l2 :: Nil)
      diffs.size should be (3)
      println(diffs)
      
      val (substMap, toSubstitute) = diffs.head
      
      toSubstitute(l) shouldBe t(l)
    }
    
    {
      val diffs = differences(e3, e4, l1 :: l2 :: Nil)
      diffs should have size (2)
      
      val (substMap, toSubstitute) = diffs.toSeq(1)
      
      toSubstitute(l) shouldBe t(l)
//      val substitutedSubExpression = ExprOps.replace(substMap, expr)
//      val newE = ExprOps.replace(substs, expr)
    }
    
    {
      val diffs = differences(e4, e5, l1 :: l2 :: Nil)
      diffs should have size (1)
      
      val (substMap, toSubstitute) = diffs.head
      
      toSubstitute(l) shouldBe t(l)
//      val substitutedSubExpression = ExprOps.replace(substMap, expr)
//      val newE = ExprOps.replace(substs, expr)
    }
    
    val allDiffs =
	    for((f1, f2) <- fragments zip fragments.tail) yield {
	      val diffs = differences(f1, f2, l1 :: l2 :: Nil).map(_._1)
	      info(s"diffs between $f1 and $f2 are $diffs")
		    diffs.toSet
	    }
    
    val intersect = allDiffs.reduce(_ intersect _)
    val union = allDiffs.reduce(_ union _)

    intersect should be ('empty)
    union should not be ('empty)
    union should contain (Map(l2 -> l2, l1 -> t(l1)))
    union should contain (Map(l1 -> l1, l2 -> t(l2)))

  }

}
