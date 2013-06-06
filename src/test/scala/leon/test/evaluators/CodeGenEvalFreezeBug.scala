package leon.test
package evaluators

import leon._

import leon.codegen.CodeGenEvalParams
import leon.evaluators._
import leon.evaluators.EvaluationResults._

import leon.plugin.{TemporaryInputPhase, ExtractionPhase}

import leon.purescala.Common._
import leon.purescala.Definitions._
import leon.purescala.Trees._
import leon.purescala.TypeTrees._

import org.scalatest.FunSuite

class CodeGenEvalFreezeBug extends FunSuite {
  private implicit lazy val leonContext = LeonContext(
    settings = Settings(
      synthesis = false,
      xlang     = false,
      verify    = false
    ),
    files = List(),
    reporter = new SilentReporter
  )

  private def parseString(str : String) : Program = {
    val pipeline = TemporaryInputPhase andThen ExtractionPhase

    val errorsBefore   = leonContext.reporter.errorCount
    val warningsBefore = leonContext.reporter.warningCount

    val program = pipeline.run(leonContext)((str, Nil))
  
    assert(leonContext.reporter.errorCount   === errorsBefore)
    assert(leonContext.reporter.warningCount === warningsBefore)

    program
  }

  test("Bug") {
    val p = """|object InsertionSort {
               |  sealed abstract class List
               |  case class Cons(head: Int, tail: List) extends List
               |  case class Nil() extends List
               |
               |  def size(l: List): Int = (l match {
               |    case Nil() => 0
               |    case Cons(_, xs) => 1 + size(xs)
               |  }) ensuring (_ >= 0)
               |
               |  def contents(l: List): Set[Int] = l match {
               |    case Nil() => Set.empty
               |    case Cons(x, xs) => contents(xs) ++ Set(x)
               |  }
               |
               |  def isSorted(l: List): Boolean = l match {
               |    case Nil() => true
               |    case Cons(x, Nil()) => true
               |    case Cons(x, Cons(y, ys)) => x <= y && isSorted(Cons(y, ys))
               |  }
               |
               |  def insert(e: Int, l: List): List = {
               |    require(isSorted(l))
               |    l match {
               |      case Nil() => Cons(e, Nil())
               |      case Cons(x, xs) =>
               |        if (x <= e) Cons(x, insert(e, xs))
               |        else Cons(e, l)
               |    }
               |  } ensuring (res => contents(res) == contents(l) ++ Set(e)
               |    && isSorted(res)
               |    && size(res) == size(l) + 1)
               |
               |  def sort(l: List): List = sort(insert(size(l), l))
               |  
               |  def aux() = sort(Nil())
               |
               |}""".stripMargin

    val prog = parseString(p)
    val evaluator = new CodeGenEvaluator(leonContext, prog, CodeGenEvalParams(maxFunctionInvocations = 10000, checkContracts = true))
    val auxDef = prog.definedFunctions.find(_.id.name == "aux").get
    val toEval = FunctionInvocation(auxDef, Nil)
    val closure = evaluator.compile(toEval, Nil).get

    // The bug is that used to mysteriously get stuck...
    closure(Nil) match {
      case EvaluatorError(_) => assert(true)
      case _ => assert(false, "Evaluation should be divergent, yet it terminated.")
    }
  }
}
