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

/*
 * NOTE:
 * if we have l1, l2 that makes problem for sorting
 */
class FlattenTest extends FunSuite with Matchers with Inside with HasLogger {

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

  val problems = forFile(ioExamplesTestcaseDir + "Flatten.scala").toList
  problems.size should be (3)

  val (sctx, funDef, problem) = problems.head

  implicit val program = sctx.program

  val consClass = program.caseClassDef("Cons").typed
  val nilClass = program.caseClassDef("Nil").typed
  val nilExp = CaseClass(nilClass, Nil): Expr

  val l :: Nil = problem.as.map(_.toVariable)

  def t(expr: Expr) = {
    caseClassSelector(consClass, expr, consClass.fields.find(_.id.name == "tail").get.id)
    //      CaseClassSelector(consClass, expr, consClass.fields.find(_.id.name == "tail").get.id)
  }

  val extraction = new ExamplesExtraction(sctx, sctx.program)
  val examples = extraction.extract(problem)
  test("num of examples") {
    withClue(examples) {
      examples.size should be(3)
    }
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
      val hints = ws.collect { case Hint(e) if formulaSize(e) >= 4 => e }
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
       |} else if ((l == Nil)) {
       |  IntCons(l.head.fst, IntCons(l.head.snd, ?(l.tail)))
       |} else {
       |  ()
       |}""".stripMargin
  
  ignore("test synthesizer, normal case") {

    val ((inIds, outId), transformedExamples) = ExamplesExtraction.transformMappings(examples).get
    //      info(s"inIds $inIds")
    val unorderedFragments = Fragmenter.constructFragments(transformedExamples, inIds)

    val synthesizer = new Synthesizer

    val Some((body, funDef)) = synthesizer.synthesize(examples.toList, _ => getEnum, evaluator, nilClass)

    info("(body, funDef) is: " + (body, funDef))
    body.toString shouldBe correctFunctionString
  }
  
  ignore("test synthesizer for variying #examples") {
    
    val numsOfExamples =
      for ( (sctx, funDef, problem) <- problems) yield {
        implicit val program = sctx.program
      
        val l :: Nil = problem.as.map(_.toVariable)
      
        val extraction = new ExamplesExtraction(sctx, sctx.program)
        val examples = extraction.extract(problem)
    
        val ((inIds, outId), transformedExamples) = ExamplesExtraction.transformMappings(examples).get
        //      info(s"inIds $inIds")
        val unorderedFragments = Fragmenter.constructFragments(transformedExamples, inIds)
    
        val synthesizer = new Synthesizer
    
        val Some((body, funDef)) = synthesizer.synthesize(examples.toList, _ => getEnum, evaluator, nilClass)
    
        info("(body, funDef) is: " + (body, funDef))
        body.toString shouldBe correctFunctionString
        
        examples.size
      }
    
    numsOfExamples should contain allOf (3, 4, 5)

  }

}