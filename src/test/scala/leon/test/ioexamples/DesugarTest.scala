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
import java.io._
import java.nio.file._

class DesugarTest extends FunSuite with Matchers with Inside with HasLogger {

  import Scaffold._
  import Constructors._
  import UtilCaseClass._

  import ExampleInputs._
  import Differencer.{ differences }
  import Util.w

  val ioExamplesTestcaseDir = "testcases/synthesis/io-examples/"

  //    println(mapOfSubexpressions(f3))
  import scala.language.implicitConversions
  import Util.{ diffsToString, diffToString }

  implicit def expressionsToString(l: Iterable[Expressions.Expr]) =
    l.map({ case a => CustomPrinter(a) }).mkString("\n")

  val problems = forFile(ioExamplesTestcaseDir + "Desugar.scala").toList

  test("check synthesis problem") {
    problems.size should be(1)
  }

  val (sctx, funDef, problem) = problems.head

  implicit val program = sctx.program

  test("check environment") {
    withClue(program.definedClasses) {
      program.definedClasses.map(_.id.name) should contain("BoolLiteral")
      program.definedClasses.filter(_.id.name == "BoolLiteral").size shouldBe 1
      program.definedClasses.filter(_.id.name == "Plus").size shouldBe 2

      program.modules.find(_.id.name == "Trees").get.
        definedClasses.filter(_.id.name == "Plus").size shouldBe 1
    }
  }

  val treesModule = program.modules.find(_.id.name == "Trees").get
  val desugarModule = program.modules.find(_.id.name == "Desugar").get

  //  val evaluator = new DefaultEvaluator(sctx, program)
  val evaluator = new CodeGenEvaluator(sctx, program)

  import scife._
  import enumeration.{ Map => _, _ }
  import dependent._
  import memoization._
  import scife.{ enumeration => e }
  import scife.util._

  val firstLiterals =
    Map(
      //        "int" -> (1 :: 3 :: 5 :: Nil).map(x => CaseClass(gc("Literal").typed, IntLiteral(x) :: Nil)),
      "int" -> (1 :: 3 :: 5 :: Nil).map(
        x => CaseClass(treesModule.definedClasses.find(_.id.name == "IntLiteral").get.asInstanceOf[CaseClassDef].typed, IntLiteral(x) :: Nil)),
      "bool" -> (true :: Nil).map(
        x => CaseClass(treesModule.definedClasses.find(_.id.name == "BoolLiteral").get.asInstanceOf[CaseClassDef].typed, BooleanLiteral(x) :: Nil)))

  val secondLiterals =
    Map(
      //        "int" -> (1 :: 3 :: 5 :: Nil).map(x => CaseClass(gc("Literal").typed, IntLiteral(x) :: Nil)),
      "int" -> (1 :: 3 :: 5 :: Nil).map(
        x => CaseClass(desugarModule.definedClasses.find(_.id.name == "Literal").get.asInstanceOf[CaseClassDef].typed, IntLiteral(x) :: Nil)),
      "bool" -> (0 :: Nil).map(
        x => CaseClass(desugarModule.definedClasses.find(_.id.name == "Literal").get.asInstanceOf[CaseClassDef].typed, IntLiteral(x) :: Nil)))

  val firstASTops = Map(
    ("int" -> ("Plus" :: "Ite" :: Nil)),
    ("bool" -> Nil))

  val secondsASTops = Map(
    ("int" -> ("Plus" :: "Ite" :: Nil)),
    ("bool" -> ("CheckType" :: Nil)))

  def fromOpToOperandParam(op: String) =
    op match {
      case "Plus" =>
        ("int", "int")
      case "Ite" =>
        //                (leftSize, "bool" :: "int" :: Nil)
        // restrict to all ints
        ("bool", "int")
      //      case "And" =>
      //        (leftSize, "int" :: Nil)
      case "CheckType" =>
        ("int", "int")
    }

  def constructEnumerator(ops: String => List[String], opToParams: String => (String, String),
                          moduleDef: ModuleDef, literals: Map[String, List[CaseClass]])(implicit ms: MemoizationScope) = {
    import enumeration.dependent._

    def gc(name: String) = {
      moduleDef.definedClasses.find(_.id.name == name).get.asInstanceOf[CaseClassDef]
    }

    val treesOfSize: Depend[(Int, List[String]), Expr] = Depend.memoized(
      (self: Depend[(Int, List[String]), Expr], pair: (Int, List[String])) => {
        val (size, types) = pair

        if (size == 0) e.Empty
        else if (size == 1) {
          e.WrapArray(types.map(literals).flatten.toArray): Finite[Expr]
        } else {
          val roots: Finite[String] = e.Enum(types.map(ops).flatten)
          val leftSizes: Finite[Int] = e.WrapArray(1 until size)

          val rootLeftSizePairs = e.Product(leftSizes, roots)

          val leftTrees: Depend[(Int, String), Expr] = InMap(self, { par: ((Int, String)) =>
            val (leftSize, rootColor) = par
            (leftSize, opToParams(rootColor)._1 :: Nil)
          })

          val rightTrees: Depend[(Int, String), Expr] = InMap(self, { par: ((Int, String)) =>
            val (leftSize, rootColor) = par
            (size - leftSize - 1, opToParams(rootColor)._2 :: Nil)
          })

          val leftRightPairs: Depend[(Int, String), (Expr, Expr)] =
            Product(leftTrees, rightTrees)

          val allNodes =
            memoization.Chain[(Int, String), (Expr, Expr), Expr](rootLeftSizePairs, leftRightPairs,
              (p1: (Int, String), p2: (Expr, Expr)) => {
                val ((leftSize, rootColor), (leftTree, rightTree)) = (p1, p2)

                CaseClass(gc(rootColor).typed, leftTree :: rightTree :: Nil)
              })

          allNodes
        }
      })

    treesOfSize
  }

  def getFirstASTs(size: Int) = {
    val ms = new scope.AccumulatingScope
    val enum = constructEnumerator(firstASTops, fromOpToOperandParam _, treesModule, firstLiterals)(ms)

    for (
      currSize <- 1 to size;
      e = enum.getEnum(currSize, "int" :: "bool" :: Nil);
      ind <- 0 until e.size
    ) yield e(ind)
  }

  def getSecondASTs(size: Int) = {
    val ms = new scope.AccumulatingScope
    val enum = constructEnumerator(secondsASTops, fromOpToOperandParam _, desugarModule, secondLiterals)(ms)

    for (
      currSize <- 1 to size;
      e = enum.getEnum(currSize, "int" :: "bool" :: Nil);
      ind <- 0 until e.size
    ) yield e(ind)
  }

  test("datastructure generation") {

    //    getFirstASTs(1) should have size 5
    //    getFirstASTs(3) should have size 5
    //    getFirstASTs(5) should have size 186

  }

  test("datastructure generation, static typed") {

    //    getSecondASTs(3) should have size 5
    //    getSecondASTs(7) should have size 2913
    getSecondASTs(7).map(_.toString) should
      contain("Ite(CheckType(Literal(1), Literal(3)), Plus(Literal(1), Literal(3)))")

  }

  test("inputs") {
    problem.xs should have size 1

    val resType = problem.xs.head.getType

    info("going into enumeration")

    val examples = getFirstASTs(3)

    //    "output finding" -
    {
      val phi = problem.phi

      problem.as should have size 1
      val in = problem.as.head

      problem.xs should have size 1
      val out = problem.xs.head

      val results = new collection.mutable.MutableList[(Expr, Expr)]()
      val filteredExamples =
        {
          val phi = problem.phi

          problem.as should have size 1
          val in = problem.as.head
          problem.xs should have size 1
          val out = problem.xs.head
          val pc = problem.pc
          val toEvaluate = pc.toClause
          val compiled = evaluator.compile(toEvaluate, in :: Nil).get

          for (ex1 <- examples) yield {
            val res = compiled(new Model(Map(in -> ex1)))

            res match {
              case EvaluationResults.Successful(BooleanLiteral(v)) if v =>
                info("pass precondition for: " + ex1)
                Some(ex1)
              case e: EvaluationResults.EvaluatorError =>
                None
              case _ =>
                None
            }
          }

        } flatten

      filteredExamples.size should be === examples.size

      val secondAstExamples = getSecondASTs(7)

      val pc = problem.pc
      val toEvaluate = And(pc.toClause, phi)
      val compiled = evaluator.compile(toEvaluate, in :: out :: Nil).get
      for (
        ex1 <- filteredExamples;
        ex2 <- secondAstExamples
      ) {
        //        info("toEvaluate " + toEvaluate)

        //        info(s"for in $ex1, out $ex2")
        //          val res = evaluator.eval(toEvaluate, new Model(Map(in -> ex1, out -> ex2)))
        val res = compiled(new Model(Map(in -> ex1, out -> ex2)))
        //          info(s"for in $ex1, out $ex2, got $res")

        res match {
          case EvaluationResults.Successful(BooleanLiteral(v)) if v =>
            //            info(s"for in $ex1, out $ex2")
            //            info("***")
            //              info(s"for input $ex1\n, output $ex2\n existing result is ${results.getOrElse(ex1, w)}\n") 
            //              withClue(s"for input $ex1\n, output $ex2\n existing result is ${results.getOrElse(ex1, w)}\n") {
            //                results.getOrElse(ex1, ex2) shouldBe ex2
            //              results should not contain key (ex1)
            //              }

            //              assert(!(results contains ex1))
            //              assert(results.getOrElse(ex1, ex2))
            //              results(ex1) += ex2
            results += ((ex1, ex2))
          //          info(s"$v for $ex, ${v1}, $v2")
          case e: EvaluationResults.EvaluatorError =>
          //          info("evaluation failure: " + e + s" for $v1 and $v2")
          case _                                   =>
        }
      }

      results.size shouldBe >(0)
      info(results.map(p => "in : " + p._1 + "\nout: " + p._2).mkString("\n"))
      results should have size (26)
      results.map(_.toString).distinct should have size (26)

      val extraction = new ExamplesExtraction(sctx, sctx.program)

      val examplesIn = results.map({
        case (inEx, outEx) =>
          ((in, inEx) :: Nil, (out, outEx))
      }).toList

      // get fragments
      val ((inIds, outId), transformedExamples) = ExamplesExtraction.transformMappings(examplesIn).get
      info(s"inIds $inIds")
      info("transformed examples: " + transformedExamples.mkString("\n"))
      val unorderedFragments = Fragmenter.constructFragments(transformedExamples, inIds)
      info("unordered fragments: " + unorderedFragments.mkString("\n"))

      val unorederdFragmentsSet = unorderedFragments.toSet
      info("unorederdFragmentsSet:\n" + unorederdFragmentsSet.mkString("\n"))

      transformedExamples should have size unorderedFragments.size
      val zipped = (transformedExamples zip unorderedFragments)

      val zippedSorted = zipped.sortBy(p => ExprOps.formulaSize(p._2))

      info("" + ioexamples.Util.mapOfSubexpressionsToPathFunctions(zippedSorted.last._1._1.head).map(
        { case (k, v) => "" + k + "\n" + v(w) }))
      info("" + zippedSorted.last)

      val taken = new collection.mutable.ListBuffer[((List[Expressions.Expr], Expressions.Expr), Expressions.Expr)]()
      var covering = transformedExamples.map(_._1).toSet

      val coveringPairs =
        zippedSorted.takeWhile({
          case _ if covering.isEmpty =>
            false
          case p @ (exPair, fragment) =>
            covering = covering - exPair._1
            taken += p
            true
        })

      info(s"zipped size ${zipped.size}; coveringPairs size: " + coveringPairs.size)
      info("covering pairs\n: " + coveringPairs.mkString("\n"))

      val groupped = zipped.groupBy(_._1._1)
      val sorted =
        for ((input, list) <- groupped) yield {
          val fragments = list.map(_._2)
          (input, fragments.sortBy(ExprOps.formulaSize _).head, fragments.toSet)
        }

      info("")
      info("")
      info("")
      info("sorted:\n" + sorted.
        map({ case (k, v, _) => k.head + "\n" + v }).mkString("\n******\n"))

      info("fragments set from sorted:\n" + sorted.map(_._2).toSet.mkString("\n"))

      //      info("unordered fragments set:\n" + (transformedExamples zip unorderedFragments).
      //        map({ case (k, v) => k._1.head + "\n" + k._2 + "\n" + v }).mkString("\n******\n"))
      //      info("unordered fragments set:\n" + unorderedFragments.toSet.mkString("\n\n\n"))

      val inputsPerPredicate =
        for ((examplePair, fragment) <- zipped) yield {
          val (_, fragmentHead, _) = sorted.find(_._3 contains fragment).get

          (fragmentHead, examplePair)
        }

      val inputsPerPredicateMap =
        (Map[Expr, Set[InputOutputExampleVal]]() /: inputsPerPredicate) {
          case (current, (fragment, pair)) =>
            current + (fragment -> (current.getOrElse(fragment, Set[InputOutputExampleVal]()) + pair))
        }

      inputsPerPredicateMap.size shouldBe 7

      val (examplesNew, fragments) =
        (for (
          fragment <- inputsPerPredicateMap.keys.toList;
          example <- inputsPerPredicateMap(fragment).toList
        ) yield (example, fragment)).unzip

      info("examples (trim 10): " + examplesNew.take(10).mkString("\n"))
      info("fragments (trim 10): " + fragments.take(10).mkString("\n"))
      
//      val result =
//        synthesizer.synthesize(examples.toList, getEnum, evaluator, program.caseClassDef("Empty").typed, Some(fragments.toList))

    }

  }

}
