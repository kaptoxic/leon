package lesynth

import org.junit.Assert._
import org.junit.{ Test, Ignore }

import testutil.TestConfig

import leon.{ Main => LeonMain, Reporter, DefaultReporter, SilentReporter, Settings, LeonContext }
import leon.solvers.{ Solver, TimeoutSolver }
import leon.solvers.z3.{ FairZ3Solver }
import leon.verification.AnalysisPhase
import leon.plugin.ExtractionPhase

class SynthesizerTest {

  import TestConfig.lesynthTestDir
  
  @Test
  def testConcat1 {
    val synthesizer = new Synthesizer(lesynthTestDir + "ListOperationsHole.scala", reporter = new SilentReporter)
    
    val report = synthesizer.synthesize
    assertTrue(report.isSuccess)
    println(report.summaryString)
  }
  
  @Test
  def testConcat2 {
    val synthesizer = new Synthesizer(lesynthTestDir + "ListOperationsHole.scala", 5, 2, 1)
    
    val report = synthesizer.synthesize
    assertTrue(report.isSuccess)
    println(report.summaryString)
  }
  
  @Ignore
  @Test
  def testInsertionSortSort {
    val synthesizer = new Synthesizer(lesynthTestDir + "InsertionSortHoleSort.scala", 5, 2, 1)
    
    val report = synthesizer.synthesize
    assertTrue(report.isSuccess)
    println(report.summaryString)
  }
  
  @Ignore
  @Test
  def testAnalyzeProgram {
    val fileName = lesynthTestDir + "InsertionSortFull.scala"
      
    import leon.Main._
    
    {	    
	    val reporter = new SilentReporter
	      
	    val args = Array(fileName, "--timeout=2")	    	
	    val ctx = processOptions(reporter, args.toList)
	    	    
    	val program = ExtractionPhase.run(ctx)(fileName :: Nil)
	    
	    val report = AnalysisPhase.run(ctx)(program)
	    
	    assertEquals(6, report.totalConditions)
	    assertEquals(0, report.totalUnknown)
	    assertEquals(6, report.totalValid)
	    assertEquals(Some(true), Globals.allSolved)
    }
    
    {
	    val reporter = new SilentReporter
	      
	    val args = Array(fileName, "--timeout=2", "--functions=sort")	    	
	    val ctx = processOptions(reporter, args.toList)
	    	    
    	val program = ExtractionPhase.run(ctx)(fileName :: Nil)
	    
	    val report = AnalysisPhase.run(ctx)(program)
	    
	    assertEquals(2, report.totalConditions)
	    assertEquals(2, report.totalValid)
	    assertEquals(0, report.totalUnknown)
	    assertEquals(Some(true), Globals.allSolved)
    }
    
    {
	    val reporter = new SilentReporter
	      
	    val args = Array(fileName, "--timeout=2", "--functions=sort:sortedIns")	    	
	    val ctx = processOptions(reporter, args.toList)
	    	    
    	val program = ExtractionPhase.run(ctx)(fileName :: Nil)
	    
	    val report = AnalysisPhase.run(ctx)(program)
	    
	    assertEquals(4, report.totalConditions)
	    assertEquals(4, report.totalValid)
	    assertEquals(0, report.totalUnknown)
	    assertEquals(Some(true), Globals.allSolved)
    }
    
    {
	    val reporter = new SilentReporter
	      
	    val args = Array(fileName, "--timeout=2", "--functions=somethingUnknown")	    	
	    val ctx = processOptions(reporter, args.toList)
	    	    
    	val program = ExtractionPhase.run(ctx)(fileName :: Nil)
	    
	    val report = AnalysisPhase.run(ctx)(program)
	    
	    assertEquals(0, report.totalConditions)
	    assertEquals(0, report.totalValid)
	    assertEquals(0, report.totalUnknown)
	    // !!!
	    assertEquals(Some(true), Globals.allSolved)
    }
  }
  
}