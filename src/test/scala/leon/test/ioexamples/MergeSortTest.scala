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
  import UtilCaseClass._

  import ExampleInputs._
  import Differencer.{ differences }
  import Util.w

  val ioExamplesTestcaseDir = "testcases/synthesis/io-examples/"


  test("playing") {
    
//    println(mapOfSubexpressions(f3))
    import scala.language.implicitConversions
    implicit def diffsToString(l: Iterable[(Map[Expressions.Variable, Expressions.Expr],
      Expressions.Expr => Expressions.Expr)]) = l.map({case (a, b) => (a, b(w))}).mkString("\n")
    def println(s: String) = scala.Predef.println(s)
    implicit def expressionsToString(l: Iterable[Expressions.Expr]) =
      l.map({case a => CustomPrinter(a)}).mkString("\n")
    
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
    
    
    // get fragments
    val ((inIds, outId), transformedExamples) = extraction.transformMappings(examples).get
    info(s"inIds $inIds")
    val unorderedFragments = Fragmenter.constructFragments(transformedExamples, inIds)
    
    info("transformed examples: " +transformedExamples.mkString("\n"))
    info("unordered fragments: " + unorderedFragments.mkString("\n"))
    info("unordered fragments: " + unorderedFragments.map(CustomPrinter(_)).mkString("\n"))
    
    val l1 :: l2 :: Nil = problem.as.map(_.toVariable)
    
//    [h(l1),h(l2)t(l1)]
//    [h(l2)l1]
//    l1
//    [h(l2),h(l1),ht(l2)t(l1)]
//    nil
    val (f4 :: f3 :: f2 :: f5 :: f1 :: Nil) = unorderedFragments
    val fragments = f1 :: f2 :: f3 :: f4 :: f5 :: Nil
    val fragmentNames =
      (fragments zip (1 to 5).map("f" + _)).toMap
      
    // kick out fragments that cannot be compared ?
    val crucialFragments = f2 :: f3 :: f4 :: f5 :: Nil
    for (crucialFragmentsPerm <- crucialFragments.permutations) {
      val sortedFragments = Util.sort(crucialFragmentsPerm)
      withClue(s"${sortedFragments: String} != ${crucialFragments: String}") {
        sortedFragments shouldBe crucialFragments
      }
    }
    
    info("ordered fragments: " + fragments.mkString("\n"))
    info("ordered fragments: " + fragments.map(CustomPrinter(_)).mkString("\n"))
    
    {
      // comparing l1 and nil should return nothing
      val diffs = differences(f1, f2, l1 :: l2 :: Nil)
      diffs should be ('empty)
    }
    
    {
      val diffs = differences(f2, f3, l1 :: l2 :: Nil)
      diffs.size should be (3)
      
      val (substMap, toSubstitute) = diffs.head
      
      diffs.find(_._1 == Map(l1 -> l1)).get._2(l) shouldBe CaseClass(consClass,
        caseClassField(l2, "head") :: l :: Nil)
    }
    
//    {
//      val diffs = differences(f3, f4, l1 :: l2 :: Nil)
//      diffs should have size (2)
//      
//      val (substMap, toSubstitute) = diffs.toSeq(1)
//      
//      val validDiffs = diffs.filter(!Differencer.areCompatible(_, null).isEmpty)
//      diffs.filter should contain (CaseClass(consClass, caseClassField(l2, "head") :: l :: Nil))
////      val substitutedSubExpression = ExprOps.replace(substMap, expr)
////      val newE = ExprOps.replace(substs, expr)
//    }
//    
//    {
//      val diffs = differences(f4, f5, l1 :: l2 :: Nil)
//      diffs should have size (1)
//      
//      val (substMap, toSubstitute) = diffs.head
//      
//      toSubstitute(l) shouldBe t(l)
////      val substitutedSubExpression = ExprOps.replace(substMap, expr)
////      val newE = ExprOps.replace(substs, expr)
//    }
    
    {
      val allDiffs =
  	    for((f1, f2) <- fragments zip fragments.tail) yield {
  	      val diffs = differences(f1, f2, l1 :: l2 :: Nil).map(_._1)
  		    diffs.toSet
  	    }
      
      val intersect = allDiffs.reduce(_ intersect _)
      val union = allDiffs.reduce(_ union _)
  
      intersect should be ('empty)
      union should not be ('empty)
      union should contain (Map(l2 -> l2, l1 -> t(l1)))
      union should contain (Map(l1 -> l1, l2 -> t(l2)))
      
      allDiffs.tail.count(_ contains Map(l1 -> l1, l2 -> t(l2))) shouldBe 1
      allDiffs.tail.count(_ contains Map(l2 -> l2, l1 -> t(l1))) shouldBe 1
    }
    
    val allDiffs =
	    for((f1, f2) <- fragments zip fragments.tail) yield {
	      val diffs = differences(f1, f2, l1 :: l2 :: Nil)
	      fine(s"diffs between $f1 and $f2 are ${diffs: String}")
	      (f1, f2, diffs)
	    }
    
    val compatibles =
      for ((f11, f21, diffs1) <- allDiffs.tail;
        (f12, f22, diffs2) <- allDiffs.drop(2);
        if f11 != f12;
        diff1 <- diffs1;
        diff2 <- diffs2;
        _ = finer(s"Checking diffs: $diff1 and $diff2");
        merged <- Differencer.areCompatible(diff1, diff2);
        _ = finer(s"compatible!!")
      ) yield (f11 :: f21 :: f12 :: f22 :: Nil map fragmentNames, merged)
        
    info("compatibles: " + compatibles.map(p => (p._1, (p._2._1, p._2._2(w)))).mkString("\n"))

    compatibles should have size 1
    inside(compatibles.head) {
      case (list, (map, fun)) =>
        list shouldBe List("f2", "f3", "f4", "f5")
        map shouldBe Map(l1 -> l1, l2 -> t(l2))
    }
   
    
  }

}
