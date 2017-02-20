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
    problems.size should be(1)
  }

  val (sctx, funDef, problem) = problems.head

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
        FunctionCalls(sctx.program, sctx.functionContext, inputs.map(_.getType), exclude) ||
        recCalls
    }

    val termSynthesizer = new TermSynthesizer(sctx, problem, inGrammar = Some(myGrammar), sizes = (6, 15))

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
    
    val firstN = enum.take(800).toList
    
    info("firstN:\n" + firstN.zipWithIndex.mkString("\n"))
  }
}
