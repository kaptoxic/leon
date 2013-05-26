package lesynth

import scala.collection.mutable.{ Map => MutableMap }
import scala.collection.mutable.{ Set => MutableSet }
import scala.collection.mutable.{ LinkedList => MutableList }

import leon.{ Main => LeonMain, DefaultReporter, Settings, LeonContext }
import leon.evaluators._
import leon.evaluators.EvaluationResults._
import leon.solvers.Solver
import leon.solvers.z3.{ FairZ3Solver }
import leon.verification.AnalysisPhase
import leon.purescala.TypeTrees.{ TypeTree => LeonType, _ }
import leon.purescala.Trees._
import leon.purescala.Definitions.{ FunDef, VarDecl, Program, ObjectDef }
import leon.purescala.Common.{ Identifier, FreshIdentifier }
import leon.purescala.TreeOps

import insynth.util.logging.HasLogger

import lesynth.examples.Example

import EvaluationResults._

class ExampleRunner(program: Program, maxSteps: Int = 2000) extends HasLogger {

  import TreeOps._

  val leonEmptyContext = LeonContext()

  // its okay to construct just one, prog is not use in the default evaluator
  lazy val defaultEvaluator = {
    val ev = new DefaultEvaluator(leonEmptyContext, program)
    ev.maxSteps = maxSteps
    ev
  }
  
  def evaluate(expr: Expr, example: Example) = {
    val mapping = example.getMapping
    fine("to evaluate: " + expr + " for mapping: " + mapping)
    defaultEvaluator.eval(expr, mapping) match {
      case Successful(BooleanLiteral(true)) =>
        fine("Eval succeded: EvaluationSuccessful(true)")
        true
      case m =>
        fine("Eval failed: " + m)
        false
    }
  }

  def evaluate(expr: Expr, mapping: Map[Identifier, Expr]) = {
    fine("to evaluate: " + expr + " for mapping: " + mapping)
    defaultEvaluator.eval(expr, mapping) match {
      case Successful(BooleanLiteral(true)) =>
        fine("Eval succeded: EvaluationSuccessful(true)")
        true
      case m =>
        fine("Eval failed: " + m)
        false
    }
  }

  def evaluateToResult(expr: Expr, mapping: Map[Identifier, Expr]) = {
    fine("to evaluate: " + expr + " for mapping: " + mapping)
    defaultEvaluator.eval(expr, mapping)
  }

  /** filter counterexamples according to an expression (precondition) */
  def filter(prec: Expr, counterExamples: Seq[Example]) = {
    entering("filter(" + prec + ")")
    fine("Old counterExamples.size: " + counterExamples.size)
    val filteredCounterExamples = counterExamples filter {
      evaluate(prec, _)
    }
    fine("New counterExamples.size: " + counterExamples.size)
    filteredCounterExamples
  }

  /** count how many examples pass */
  def countPassed(expressionToCheck: Expr, givenCounterExamples: Iterable[Example]):
  	(List[Example], List[Example]) = {
    fine("expressionToCheck: " + expressionToCheck)

    ((Nil: List[Example], Nil: List[Example]) /: givenCounterExamples) {
      case ((passed, failed), ce) =>
        {
          if (evaluate(expressionToCheck, ce)) (passed :+ ce, failed)
          else (passed, failed :+ ce)
        }
    }
  }
  
  def countPassedForBody(body: Expr, postcondition: Expr, givenCounterExamples: Iterable[Example]):
  	(List[Example], List[Example]) = {
    fine("checking body: " + body)

    ((Nil: List[Example], Nil: List[Example]) /: givenCounterExamples) {
      case ((passed, failed), ce) =>
        {
          if (evaluate(ce.getExpression(body, postcondition), ce)) (passed :+ ce, failed)
          else (passed, failed :+ ce)
        }
    }
  }
  
//  def countPassedAndTerminating(body: Expr): Int = {
//    // TODO body dont have set type in synthesizer
//    val expressionToCheck =
//      //Globals.bodyAndPostPlug(exp)
//      {
//        val resFresh = FreshIdentifier("result", true).setType(body.getType)
//        Let(
//          resFresh, body,
//          replace(Map(ResultVariable() -> Variable(resFresh)), matchToIfThenElse(holeFunDef.getPostcondition)))
//      }
//    fine("expressionToCheck: " + expressionToCheck)
//
//    (0 /: counterExamples) {
//      (res, ce) =>
//        {
//          evaluateToResult(expressionToCheck, ce) match {
//            case EvaluationSuccessful(BooleanLiteral(true)) =>
//              res + 1
//            case EvaluationError("Stack overflow") =>
//              return -counterExamples.size
//            case m =>
//              res
//          }
//        }
//    }
//  }

  // check if this body passes all examples
//  def check(body: Expr): Boolean = {
//    val examplesToCheck = counterExamples
//    val expressionToCheck = Globals.bodyAndPostPlug(body)
//    fine("Expression to check: " + expressionToCheck)
//
//    var res = true
//    val iterator = counterExamples.iterator
//
//    while (res && iterator.hasNext) {
//      val currentExample = iterator.next
//      res = evaluate(expressionToCheck, currentExample)
//
//      if (!res) fine("Failed example: " + currentExample)
//    }
//
//    return res
//  }

}