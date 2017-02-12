/* Copyright 2009-2014 EPFL, Lausanne */

package leon.test.condabd
package util

import java.io.{BufferedWriter, FileWriter, File}

import org.scalatest.Assertions._

import leon._
import leon.test._
import leon.frontends._
import leon.utils._
import leon.purescala.Definitions._
import leon.purescala.Expressions._
import leon.purescala.ExprOps._
import leon.solvers.z3._
import leon.solvers.Solver
import leon.synthesis._
import leon.synthesis.utils._

object Scaffold {

  val reporter = new TestSilentReporter
  
//  val parametersToLeon = "--synthesis" :: "--strictCompilation=false" :: Nil
  // debug prevents printing numbers after ids
  val parametersToLeon = "--synthesis" :: "--debug=solver" :: Nil

  val forProgramReporter = new TestSilentReporter
  val ctx = Main.processOptions(parametersToLeon).copy(
    files = List(),
    reporter = forProgramReporter,
    interruptManager = new InterruptManager(forProgramReporter),
    options = Main.processOptions(parametersToLeon).options diff
      Seq(LeonOption[Set[DebugSection]](GlobalOptions.optDebug)(Set(DebugSectionTrees)))
  )

  def forProgram(content: String): Iterable[(SynthesisContext, FunDef, Problem)] = {

    val pipeline = TemporaryInputPhase andThen
      scalac.ExtractionPhase andThen
      new leon.utils.PreprocessingPhase andThen
      SynthesisProblemExtractionPhase

    val (newCtx, (program, results)) = try {
      pipeline.run(ctx, (content :: Nil, Nil))
    } catch {
      case _: Throwable =>
        fail("Error while processing")
    }
    
    extractProblems(newCtx, program, results)
  }
  
  def forProgramDefinitions(content: String) = {

    val pipeline = TemporaryInputPhase andThen
      scalac.ExtractionPhase andThen
      new leon.utils.PreprocessingPhase

    val (newCtx, program) = try {
      pipeline.run(ctx, (content :: Nil, Nil))
    } catch {
      case _: Throwable =>
        fail("Error while processing")
    }
    
    (newCtx, program)
  }

  def forFile(file: String): Iterable[(SynthesisContext, FunDef, Problem)] = {
    val programFile = new File(file)

//    val pipeline = scalac.ExtractionPhase andThen SynthesisProblemExtractionPhase
//    
//    val (newCtx, (program, results)) = try {
//      pipeline.run(ctx, file :: Nil)
//    } catch {
//      case e: Throwable =>
//        fail(s"Error $e, while processing " + file)
//    }
    
    val pipeline = leon.frontends.scalac.ExtractionPhase andThen new leon.utils.PreprocessingPhase
    
    val (newCtx, program) = try {
      pipeline.run(ctx, file :: Nil)
    } catch {
      case LeonFatalError(msg) =>
        fail(s"Compilation failed for file $file: ${msg.getOrElse("")}")
//        throw new Exception(s"Compilation failed for file $file: ${msg.getOrElse("")}")
    }
    
    val chooses = SourceInfo.extractFromProgram(newCtx, program)
    val synthesisSettings = SynthesisSettings()
    
    for (ci <- chooses) yield {
      val sctx = new SynthesisContext(newCtx,
                                      synthesisSettings,
                                      ci.fd,
                                      program)

      (sctx, ci.fd, ci.problem)
    }
    
//    extractProblems(newCtx, program, results)
  }
  
  private def extractProblems(ctx: LeonContext, program: Program, 
    problemMap: Map[leon.purescala.Definitions.FunDef, Seq[SourceInfo]]) = {

    // options became settings, settings became options...
//    val opts = SynthesisOptions()
    
    val synthesisSettings =
      SynthesisSettings(
//        rules = Nil
      )

    for ((f, ps) <- problemMap; sourceInfo <- ps) 
    	yield {
        val sctx = new SynthesisContext(ctx,
                                        synthesisSettings,
                                        f,
                                        program)

        (sctx, f, sourceInfo.problem)
    	}
  }
  
}
