package leon.test
package ioexamples

import leon._
import leon.purescala.Definitions._
import leon.purescala.Trees._
import leon.purescala.TreeOps._
import leon.solvers.z3._
import leon.solvers.Solver
import leon.synthesis._
import leon.synthesis.utils._
import leon.synthesis.ioexamples._
import purescala.Extractors._

import leon.test.condabd.util._

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers._

import java.io.{ BufferedWriter, FileWriter, File }

class SExpressionTransformerTest extends FunSuite {

  import Scaffold._

  val lesynthTestDir = "testcases/synthesis/"
    
  val problems = forFile(lesynthTestDir + "ExamplesAsSpecifications.scala").toList
  problems.size should be (3)
  
  val (sctx, funDef, problem) = problems.head
  
  val program = sctx.program
  
  val consClass = program.caseClassDef("Cons")
  val nilClass = program.caseClassDef("Nil")
  
  val nilExp = CaseClass(nilClass, Nil): Expr
    
  import SExpressionTransformer._
  import Util._
  
  test("Lists to S-expressions") {
    transformToSExpression(nilExp, program) match {
      case Some(Atom(_)) =>
      case _ => fail
    }
    
  }
}
