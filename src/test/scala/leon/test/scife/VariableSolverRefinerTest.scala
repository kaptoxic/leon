package leon.test
package scife

import scala.util.Random

import org.scalatest.FunSpec
import org.scalatest.GivenWhenThen

import leon.purescala.Definitions._
import leon.purescala.Common._
import leon.purescala.TypeTrees._
import leon.purescala.Trees._

import util._

class VariableSolverRefinerTest extends FunSpec with GivenWhenThen {

  import Scaffold._

	val lesynthTestDir = "testcases/condabd/test/lesynth/"  
      
  describe("A variable solver refiner with list ADT") {
    
    it("should refine if condition is isEmpty()") {
      
      for ( (sctx, funDef, problem) <- forFile(lesynthTestDir + "/ListConcatWithEmpty.scala")) {
        val program = sctx.program
        val solver = sctx.solverFactory
        val reporter = sctx.reporter
        
      }
    }
    
  }
  
}
