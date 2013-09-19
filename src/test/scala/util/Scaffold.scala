package util

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers._

import java.io.{BufferedWriter, FileWriter, File}

import leon._
import leon.test._
import leon.utils._
import leon.purescala.Definitions._
import leon.purescala.Trees._
import leon.purescala.TreeOps._
import leon.solvers.z3._
import leon.solvers.Solver
import leon.synthesis._
import leon.synthesis.utils._

object Scaffold {

  def forProgram(content: String): Iterable[(SynthesisContext, FunDef, Problem)] = {

    val reporter = new TestSilentReporter
    val ctx = LeonContext(
      settings = Settings(
        synthesis = true,
        xlang     = false,
        verify    = false		
      ),
      files = List(),
      reporter = reporter,
      interruptManager = new InterruptManager(reporter)
    )
//    Settings.showIDs = true

    val pipeline = leon.plugin.TemporaryInputPhase andThen leon.plugin.ExtractionPhase andThen SynthesisProblemExtractionPhase

    val (program, results) = try {
      pipeline.run(ctx)((content, Nil))
    } catch {
      case _: Throwable =>
        fail("Error while processing")
    }
    
    extractProblems(ctx, program, results)
  }

  def forFile(file: String): Iterable[(SynthesisContext, FunDef, Problem)] = {
    val programFile = new File(file)

    val reporter = new TestSilentReporter

    val ctx = LeonContext(
      settings = Settings(
        synthesis = true,
        xlang     = false,
        verify    = false
      ),
      files = List(programFile),
      reporter = reporter,
      interruptManager = new InterruptManager(reporter)
    )

    val pipeline = leon.plugin.ExtractionPhase andThen SynthesisProblemExtractionPhase

    val (program, results) = try {
      pipeline.run(ctx)(file :: Nil)
    } catch {
      case _: Throwable =>
        fail("Error while processing " + file)
    }
    
    extractProblems(ctx, program, results)
  }
  
  private def extractProblems(ctx: LeonContext, program: Program, 
    problemMap: Map[leon.purescala.Definitions.FunDef,Seq[leon.synthesis.Problem]]) = {

    val opts = SynthesisOptions()

    for ((f, ps) <- problemMap; p <- ps) 
    	yield {
        val sctx = SynthesisContext(ctx,
                                    opts,
                                    Some(f),
                                    program,
                                    new TestSilentReporter)

        (sctx, f, p)
    	}
  }
  
}
