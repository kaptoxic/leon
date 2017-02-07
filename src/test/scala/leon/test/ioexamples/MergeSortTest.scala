package leon.test
package ioexamples

import leon._
import purescala._
import Expressions._
import Definitions._
import Types._
import leon.solvers.z3._
import leon.solvers.Solver
import leon.synthesis._
import leon.synthesis.utils._
import leon.synthesis.ioexamples._

import leon.test.condabd.util._

import org.scalatest._
import org.scalatest.Matchers._

import java.io.{ BufferedWriter, FileWriter, File }

class MergeSortTest extends FunSpec with Inside {

  import Scaffold._
  import Constructors._

  val ioExamplesTestcaseDir = "testcases/synthesis/io-examples/"

  describe("playing") {
    
    val problems = forFile(ioExamplesTestcaseDir + "MergeSortMerge.scala").toList
    problems.size should be (1)
    
    val (sctx, funDef, problem) = problems.head
    
    val extraction = new ExamplesExtraction(sctx, sctx.program)
    val examples = extraction.extract(problem)
    withClue(examples) {
      examples.size should be (5)
    }

  }

}
