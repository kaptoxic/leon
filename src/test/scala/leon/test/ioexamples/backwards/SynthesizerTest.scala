package leon.test
package ioexamples.backwards

import leon.purescala.Definitions._
import leon.purescala.Trees._
import leon.purescala.TreeOps._
import leon.solvers.z3._
import leon.solvers.Solver
import leon.synthesis.{ Synthesizer => _, _ }
import leon.synthesis.utils._
import leon.synthesis.ioexamples.backwards._
import leon.evaluators._

import leon.test.condabd.util._

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers._

import java.io.{ BufferedWriter, FileWriter, File }

class SynthesizerTest extends FunSuite {

  import Scaffold._

  val testDir = "testcases/ioexamples/"
    
  val problems = forFile(testDir + "MergeSort_Sort.scala").toList
  problems.size should be (1)
  
  type IO = (Expr, Expr)
  // may come in handy
  implicit def exprToPair(expr: Expr) = (expr, expr)
  
  {
    val (sctx, funDef, problem) = problems.head
    
    val program = sctx.program
    val inputVar = funDef.args.head.toVariable
    val codeGenEval = new CodeGenEvaluator(sctx.context, sctx.program)
    
    val consClass = program.caseClassDef("Cons")
    val nilClass = program.caseClassDef("Nil")
    val nilExp = CaseClass(nilClass, Nil): Expr

    object TestDecomposer extends (Expr => Map[Expr, Expr]) {
      def apply(expr: Expr): Map[Expr, Expr] = expr match {
        case `inputVar` =>
          Map(nilExp -> inputVar)
        case _ =>
          throw new RuntimeException
      }
    }
    
    val functions = program.definedFunctions.groupBy( f => f.returnType)
    
    val nil = nilExp
    val l0 = (CaseClass(consClass, IntLiteral(0) :: nilExp :: Nil))
  
    test("given a single example") {
      val synthesizer = new Synthesizer(codeGenEval)
      val examples: List[IO] = List( nil )
    
	    synthesizer.synthesize(inputVar.id, examples, functions, TestDecomposer) should
	      be (Some(inputVar))	    
    }
    
    test("given two examples") {
      val synthesizer = new Synthesizer(codeGenEval)
      val examples: List[IO] = List( nil, l0 )

      synthesizer.synthesize(inputVar.id, examples, functions, TestDecomposer) should
        be (Some(inputVar))     
    }
  }

}
