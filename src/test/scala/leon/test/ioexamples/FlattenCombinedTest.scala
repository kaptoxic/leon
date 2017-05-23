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

import datagen.GrammarDataGen

import leon.utils.logging._

import leon.test.condabd.util._

import org.scalatest._
import org.scalatest.Matchers._

import java.io.{ BufferedWriter, FileWriter, File }

class FlattenCombinedTest extends FunSuite with Matchers with Inside with HasLogger {

  import Scaffold._
  import Constructors._
  import UtilCaseClass._

  import ExampleInputs._
  import Differencer.{ differences }
  import Util.w

  val ioExamplesTestcaseDir = "testcases/synthesis/io-examples/"

  import scala.language.implicitConversions
  import Util.{ diffsToString, diffToString }

  implicit def expressionsToString(l: Iterable[Expressions.Expr]) =
    l.map({ case a => CustomPrinter(a) }).mkString("\n")

  val problems = forFile(ioExamplesTestcaseDir + "FlattenCombined.scala").toList
  problems.size should be (1)

  val (sctx, funDef, problem) = problems.head

  implicit val program = sctx.program

  val consClass = program.caseClassDef("Cons").typed
  val nilClass = program.caseClassDef("Nil").typed
  val nilExp = CaseClass(nilClass, Nil): Expr

  val l :: Nil = problem.as.map(_.toVariable)

  def t(expr: Expr) = {
    caseClassSelector(consClass, expr, consClass.fields.find(_.id.name == "tail").get.id)
  }

  val extraction = new ExamplesExtraction(sctx, sctx.program)
  val examples = extraction.extract(problem)
  val pc = extraction.extractPostcondition(problem)
  test("specifications") {
    withClue(examples) {
      examples.size should be (1)
    }
    pc.toString shouldBe "contents(out) == contentsP(l)"
  }
  
  def discoverTests(fd: FunDef) = {

    val maxEnumerated = 1000
    val maxValid      = 10

    val evaluator = new CodeGenEvaluator(ctx, program)

    val inputsToExample: Seq[Expr] => Example = { ins =>
      evaluator.eval(functionInvocation(fd, ins)) match {
        case EvaluationResults.Successful(res) =>
          new InOutExample(ins, List(res))
        case _ =>
          new InExample(ins)
      }
    }

    val dataGen = new GrammarDataGen(evaluator)

    val generatedTests = dataGen
      .generateFor(fd.paramIds, fd.precOrTrue, maxValid, maxEnumerated)
      .map(inputsToExample)
      .toList

    val (genPassing, genFailing) = generatedTests.partition {
      case _: InOutExample => true
      case _               => false
    }

    genPassing.map({ case e: InOutExample => e })
  }

  def getEnum = {
    import synthesis.ioexamples.backwards.TermSynthesizer

    val heads =
      caseClassSelector(consClass, l, consClass.fields.find(_.id.name == "head").get.id) ::
//        caseClassSelector(consClass, l2, consClass.fields.find(_.id.name == "head").get.id) ::
      Nil

    val myGrammar = {
      import leon.grammars._
      import purescala.ExprOps._
      import purescala.Expressions.Expr
      import purescala.Extractors.TopLevelAnds
      import purescala.Types.{ BooleanType, Int32Type, IntegerType }
      import Witnesses.Hint

      val TopLevelAnds(ws) = problem.ws
//      val hints = ws.collect { case Hint(e) if formulaSize(e) >= 4 => e }
      val inputs = /*problem.allAs.map(_.toVariable) ++ hints ++*/ heads
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

    val termSynthesizer = new TermSynthesizer(sctx, problem, inGrammar = Some(myGrammar))

    val enum = termSynthesizer.apply(BooleanType :: Nil)

    enum
  }

  val evaluator = new DefaultEvaluator(sctx, program)

  var filteredGroupsToCompare: Iterable[Option[(Expr, Iterable[(Set[(Expr, Expr)], Boolean)])]] = _
  
  val correctFunctionString =
    """|if ((l == Nil)) {
       |  IntNil
       |} else {
       |  IntCons(l.head.fst, IntCons(l.head.snd, rec(l.tail)))
       |}""".stripMargin
       
  test("automatically generated examples") {
    problem.eb.valids should have size 0
    
    val discoveredExamples = discoverTests(funDef)
    
    discoveredExamples should have size 10
  }
  
  ignore("test synthesizer, normal case") {

    val ((inIds, _), transformedExamples) = ExamplesExtraction.transformMappings(examples).get
    val unorderedFragments = Fragmenter.constructFragments(transformedExamples, inIds)

    val synthesizer = new Synthesizer
    val discoveredExamples = ExamplesExtraction.transformType(problem, discoverTests(funDef))
    val allExamples = examples ++ discoveredExamples
    allExamples should have size 10

    val Some((body, resFunDef)) = synthesizer.synthesize(allExamples.toList, _ => _ => getEnum, evaluator, nilClass)

    info("(body, funDef) is: " + (body, resFunDef))
    body.toString shouldBe correctFunctionString
  }
  
}