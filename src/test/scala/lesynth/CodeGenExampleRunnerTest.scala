package lesynth

import leon._
import leon.purescala.Definitions._
import leon.purescala.Common._
import leon.purescala.Trees._
import leon.evaluators._
import leon.plugin.ExtractionPhase

import org.scalatest.FunSuite

class CodeGenExampleRunnerTest extends FunSuite {
  
  test("evaluation test") {
	  val reporter = new DefaultReporter
	  val args = Array("testcases/lesynth-eval/Sort.scala")
	  
	  val ctx = Main.processOptions(reporter, args.toList)
	  
	  val program = ExtractionPhase.run(ctx)(args.toList)
	  
	  val evaluator = new CodeGenEvaluator(ctx, program)
	    
	  val auxFunDef = program.definedFunctions.find(_.id.name == "aux").get
	  
	  val expressionToCheck = FunctionInvocation(auxFunDef, Nil)
	  
	  val closure = evaluator.compile(expressionToCheck, Nil).get
	  
	  println("aux(Nil()): compiled, going to evaluate")
	  
	  val result = closure(Nil)
  }
  
}