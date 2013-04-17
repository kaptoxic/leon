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

import EvaluationResults._

class DefaultExampleRunner(program: Program, funDef: FunDef) extends HasLogger {

  import TreeOps._

  private var _examples = MutableList[Seq[Expr]]()
  def examples = _examples
  
  // its okay to construct just one, prog is not use in the default evaluator
  private var _evaluator: Evaluator = null
  def evaluator = _evaluator
  def evaluator_=(evaluatorIn: Evaluator) = _evaluator = evaluatorIn
  
  def addExamples(ce: Iterable[Map[Identifier, Expr]]) = {
    for (map <- ce)
	    _examples :+= {
	      for(id <- funDef.args.map(_.id)) yield
	      	map(id)
	    }
  }

  val leonEmptyContext = LeonContext()

  def evaluate(expr: Expr, mapping: Map[Identifier, Expr]): Boolean = {
    fine("to evaluate: " + expr + " for mapping: " + mapping)
    evaluator.eval(expr, mapping) match {
      case Successful(BooleanLiteral(true)) =>
        fine("Eval succeded: EvaluationSuccessful(true)")
        true
      case m =>
        fine("Eval failed: " + m)
        false
    }
  }
  
  def evaluate(expr: Expr, args: Seq[Expr]): Boolean = {
    evaluate(expr, funDef.args.map(_.id).zip(args).toMap)
  }

  def evaluateToResult(expr: Expr, mapping: Map[Identifier, Expr]) = {
    fine("to evaluate: " + expr + " for mapping: " + mapping)
    evaluator.eval(expr, mapping)
  }

  /** filter counterexamples according to an expression (precondition) */
  def filter(prec: Expr) = {
    entering("filter(" + prec + ")")
    fine("Old counterExamples.size: " + examples.size)
    _examples = _examples filter {
      evaluate(prec, _)
    }
    fine("New counterExamples.size: " + examples.size)
  }

  /** count how many examples pass */
  def countPassed(expressionToCheck: Expr) = {
    // TODO body dont have set type in synthesizer
//    val expressionToCheck =
//      //Globals.bodyAndPostPlug(exp)
//      {
//        val resFresh = FreshIdentifier("result", true).setType(body.getType)
//        Let(
//          resFresh, body,
//          replace(Map(ResultVariable() -> Variable(resFresh)), matchToIfThenElse(holeFunDef.getPostcondition)))
//      }
    fine("expressionToCheck: " + expressionToCheck)

    (0 /: examples) {
      (res, ce) =>
        {
          if (evaluate(expressionToCheck, ce)) res + 1
          else res
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