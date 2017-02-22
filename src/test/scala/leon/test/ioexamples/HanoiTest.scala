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

class HanoiTest extends FunSuite with Matchers with Inside with HasLogger {

  import Scaffold._
  import Constructors._
  import UtilCaseClass._

  import ExampleInputs._
  import Differencer.{ differences }
  import Util.w

  val ioExamplesTestcaseDir = "testcases/synthesis/io-examples/"

  val problems = forFile(ioExamplesTestcaseDir + "Hanoi.scala").toList

  test("check synthesis problem") {
    problems.size should be (1)
  }

  val (sctx, funDef, problem) = problems.head

  implicit val program = sctx.program

  test("extract examples") {
    
    val extraction = new ExamplesExtraction(sctx, sctx.program)
    val examples = extraction.extract(problem)
    withClue(examples) {
      examples.size should be (3)
    }

  }

}