package leon
package scife

import scala.util.Random

import org.scalatest._

import leon.purescala.Definitions._
import leon.purescala.Common._
import leon.purescala.TypeTrees._
import leon.purescala.Trees._

import util._

class EnumeratorSynthesizerTest extends FunSuite with GivenWhenThen {

  import Scaffold._

  val testDir = "testcases/scife/"

  test("test") {

    for ((sctx, funDef, problem) <- forFile(testDir + "/SortedList.scala")) {
      val program = sctx.program
      val solver = sctx.solverFactory
      val reporter = sctx.reporter
      
      Synthesis(sctx copy (reporter = new DefaultReporter(Settings())), funDef, problem)
      
    }

  }

}
