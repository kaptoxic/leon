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

import leon.StopwatchCollections

class CodeGenExampleRunner(val evaluatorIn: Evaluator, funDef: FunDef, maxSteps: Int = 1000) extends HasLogger {

  import TreeOps._

  private var _examples = MutableList[Seq[Expr]]()
  def examples = _examples
  
  def addExamples(ce: Iterable[Map[Identifier, Expr]]) = {
    for (map <- ce)
	    _examples :+= {
	      for(id <- funDef.args.map(_.id)) yield
	      	map(id)
	    }
  }

  val leonEmptyContext = LeonContext()

  // its okay to construct just one, prog is not use in the default evaluator
  private var _evaluator: Evaluator = null
  def evaluator = _evaluator
  def evaluator_=(evaluatorIn: Evaluator) = _evaluator = evaluatorIn
    
  def evaluate(expr: Expr, args: Seq[Expr]) {
    evaluator.eval(expr, funDef.args.map(_.id).zip(args).toMap)
  }
  
  def evaluateToResult(expr: Expr, mapping: Map[Identifier, Expr]) = {
    fine("to evaluate: " + expr + " for mapping: " + mapping)
    evaluator.eval(expr, mapping)
  }
  
  def compile(expr: Expr, ids: Seq[Identifier]) = {
    StopwatchCollections.get("Compilation").newStopwatch profile evaluator.compile(expr, ids)
  }

  def evaluate(evalClosure: Seq[Expr] => Result, args: Seq[Expr]) = {
    fine("To evaluate for: " + args.mkString(", "))
    try {
	    evalClosure(args) match {
	      case Successful(BooleanLiteral(true)) =>
	        fine("Eval succeded: EvaluationSuccessful(true)")
	        true
	      case m =>
	        fine("Eval failed: " + m)
	        false
	    }
    } catch {
      case e: StackOverflowError =>
        fine("Eval failed: " + e)
        false        
    }
  }

  /** filter counterexamples according to an expression (precondition) */
  def filter(prec: Expr) = {
    entering("filter(" + prec + ")")
    fine("Old counterExamples.size: " + examples.size)
    
    val compilationResult = compile(prec, funDef.args.map(_.id))
    
    compilationResult match {
      case Some(closure) =>    
		    _examples = _examples filter {
		      evaluate(closure, _)
		    }        
      case None =>
        throw new RuntimeException
    } 
    fine("New counterExamples.size: " + examples.size)
  }

  /** count how many examples pass */
  def countPassed(expressionToCheck: Expr) = {
    fine("expressionToCheck: " + expressionToCheck)

    val compilationResult = compile(expressionToCheck, funDef.args.map(_.id))
    
    compilationResult match {
      case Some(closure) =>    
		    (0 /: examples) {
		      (res, ce) =>
		        {
		          if (evaluate(closure, ce)) res + 1
		          else res
		        }
		    }
        
      case None =>
        throw new RuntimeException
    } 
  }

}