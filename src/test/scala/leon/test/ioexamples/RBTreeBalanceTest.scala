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

class RBTreeBalanceTest extends FunSuite with Matchers with Inside with HasLogger {

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

  val problems = forFile(ioExamplesTestcaseDir + "RedBlackTreeBalance.scala").toList
  
  test("check synthesis problem") {
    problems.size should be(2)
  }

  val (sctx, funDef, problem) = problems.find(_._2.id.name == "balanceTreeInput").get

  implicit val program = sctx.program

  def getEnum(tpe: TypeTree) = {
    import synthesis.ioexamples.backwards.TermSynthesizer

    def fields: List[CaseClassSelector] = Nil
//      caseClassSelector(consClass, l1, consClass.fields.find(_.id.name == "head").get.id) ::
//        caseClassSelector(consClass, l2, consClass.fields.find(_.id.name == "head").get.id) :: Nil

    val myGrammar = {
      import leon.grammars._
      import purescala.ExprOps._
      import purescala.Expressions.Expr
      import purescala.Extractors.TopLevelAnds
      import purescala.Types.{ BooleanType, Int32Type, IntegerType }
      import Witnesses.Hint

      val TopLevelAnds(ws) = problem.ws
      val hints = ws.collect { case Hint(e) if formulaSize(e) >= 4 => e }
      val inputs = /*problem.allAs.map(_.toVariable) ++ hints ++*/ fields
      val exclude = sctx.settings.functionsToIgnore
      val recCalls = {
        if (sctx.findOptionOrDefault(SynthesisPhase.optIntroduceRecCalls)) Empty()
        else SafeRecursiveCalls(sctx.program, problem.ws, problem.pc)
      }

      BaseGrammar ||
        Closures ||
        EqualityGrammar(Set(IntegerType, Int32Type, BooleanType) ++ inputs.map { _.getType }) ||
        OneOf(inputs) ||
        Constants(sctx.functionContext.fullBody) ||
//        FunctionCalls(sctx.program, sctx.functionContext, inputs.map(_.getType), exclude) ||
        recCalls
    }

    val termSynthesizer = new TermSynthesizer(sctx, problem, inGrammar = Some(myGrammar), sizes = (8, 10))

    val enum = termSynthesizer.apply(tpe :: Nil)

    enum
  }

  val evaluator = new DefaultEvaluator(sctx, program)

  var filteredGroupsToCompare: Iterable[Option[(Expr, Iterable[(Set[(Expr, Expr)], Boolean)])]] = _
  
  test("example extraction") {
    val eb = problem.eb
    
    info("invalids:\n" + eb.invalids.mkString("\n"))
    info("valids:\n" + eb.valids.mkString("\n"))
    
    problem.hasOutputTests shouldBe false
    
    problem.xs should have size 1
    
    val resType = problem.xs.head.getType
    
    val enum = getEnum(resType)
    
    val firstN = enum.take(1600).toList
    
    info("firstN:\n" + firstN.zipWithIndex.mkString("\n"))
    
//    "output finding" -
    {
      val phi = problem.phi
      
      problem.as should have size 1
      val in = problem.as.head

      problem.xs should have size 1
      val out = problem.xs.head

      val results = collection.mutable.Map[Expr, Expr]()      

      for (ex1 <- firstN;
//        _ = info("*******");
        ex2 <- firstN) {
        val res = evaluator.eval(phi, new Model(Map(in -> ex1, out -> ex2)))
//        info(s"for in $ex1, out $ex2, got $res")

        res match {
          case EvaluationResults.Successful(BooleanLiteral(v)) if v =>
//            info(s"for in $ex1, out $ex2")
//            info("***")
            withClue(s"for input $ex1\n, output $ex2\n existing result is: " + results.getOrElse(ex1, w)) {
              results should not contain key (ex1)
            }
            results += ex1 -> ex2
          //          info(s"$v for $ex, ${v1}, $v2")
          case e: EvaluationResults.EvaluatorError =>
          //          info("evaluation failure: " + e + s" for $v1 and $v2")
          case _ =>
        }
      }
      
      results.size shouldBe > (0)
      
    }

  }
  
}
