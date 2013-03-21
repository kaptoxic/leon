package lesynth

import org.junit.Assert._
import org.junit.{ Test, Ignore }

import testutil.TestConfig

import leon.{ Main => LeonMain, Reporter, DefaultReporter, SilentReporter, Settings, LeonContext }
import leon.solvers.{ Solver, TimeoutSolver }
import leon.solvers.z3.{ FairZ3Solver }
import leon.verification.AnalysisPhase
import leon.plugin.ExtractionPhase
import leon.purescala.TypeTrees.{ ClassType, CaseClassType }
import leon.purescala.Trees.{ IntLiteral, CaseClass }
import leon.purescala.Definitions.FunDef

import insynth.leon.HoleFinder
import insynth.leon.loader.{ HoleExtractor, LeonLoader }

class SynthesizerExamplesTest {

  import TestConfig.lesynthTestDir
  
  @Test
  def testConcat1 {
    val synthesizer = new SynthesizerExamples(
      lesynthTestDir + "ListOperationsHole.scala",
      introduceExamples = introduceTwoListArgumentsExamples
      //examples = introduceCounterExamples
    )
    
    val report = synthesizer.synthesize
    assertTrue(report.isSuccess)
    println(report.summaryString)
  }
  
  @Test
  def testConcat2 {
    val synthesizer = new SynthesizerExamples(
      lesynthTestDir + "ListOperationsHole.scala", 5, 2, 1,
      introduceExamples = introduceTwoListArgumentsExamples
      //examples = introduceCounterExamples
    )
    
    val report = synthesizer.synthesize
    assertTrue(report.isSuccess)
    println(report.summaryString)
  }
  
  @Test
  def testInsertionSortSort {
    val synthesizer = new SynthesizerExamples(
      lesynthTestDir + "InsertionSortHoleSort.scala",
      introduceExamples = introduceOneListArgumentExamples
      //examples = introduceCounterExamples
    )
    
    val report = synthesizer.synthesize
    assertTrue(report.isSuccess)
    println(report.summaryString)
  }
      
  def introduceOneListArgumentExamples(holeFunDef: FunDef, loader: LeonLoader) = {
    var argumentIds = holeFunDef.args.map(_.id)
    assertEquals(1, argumentIds.size)
    
    // list type
    val ct = argumentIds(0).getType.asInstanceOf[ClassType]
    
		val setSubclasses = loader.directSubclassesMap(ct).map(_.asInstanceOf[CaseClassType].classDef)
		assertEquals(2, setSubclasses.size)
		
		val (nilClassSet, consClassSet) = setSubclasses.partition(_.fieldsIds.size == 0)
		assertEquals(1, nilClassSet.size)
		
		val nilClass = nilClassSet.head
		val consClass = consClassSet.head
		
		var counter = 10
		def getIntLiteral = { counter+=1; IntLiteral(counter) }
		val intLiteral = IntLiteral(5)
		
		val list0 = CaseClass(nilClass, Nil)
		val list1 = CaseClass(consClass, IntLiteral(5) :: list0 :: Nil)
		val list2 = CaseClass(consClass, IntLiteral(3) :: list1 :: Nil)
		val list3 = CaseClass(consClass, IntLiteral(1) :: list2 :: Nil)
		val list32 = CaseClass(consClass, IntLiteral(10) :: CaseClass(consClass, IntLiteral(7) :: list1 :: Nil) :: Nil)
		
		val lists = List(list0, list1, list2, list3, list32)
		
		for(l1 <- lists)
      yield Map(argumentIds(0) -> l1)
    
  }
  
  def introduceTwoListArgumentsExamples(holeFunDef: FunDef, loader: LeonLoader) = {
//    val fileName = lesynthTestDir + "ListOperationsHole.scala"
//    val holeFinder = new HoleFinder(fileName)
    
    
//    holeFinder.extract match {
//      case Some((prog, hole)) =>
        
//        val holeExtractor = new HoleExtractor(prog, hole)
//        val holeFunDef = holeExtractor.extractHole match {
//          case Some((funDef, _)) => funDef
//          case _ => fail("Cannot extract holeFunDef"); null
//        }
        
//        val loader = new LeonLoader(prog, hole)
        
		    val argumentIds = holeFunDef.args.map(_.id)
		    assertEquals(2, argumentIds.size)
		    loader.hole.getType match {
	      case ct: ClassType =>
	    		val setSubclasses = loader.directSubclassesMap(ct).map(_.asInstanceOf[CaseClassType].classDef)
	    		assertEquals(2, setSubclasses.size)
	    		
	    		val (nilClassSet, consClassSet) = setSubclasses.partition(_.fieldsIds.size == 0)
	    		assertEquals(1, nilClassSet.size)
	    		
	    		val nilClass = nilClassSet.head
	    		val consClass = consClassSet.head
	    		
	    		var counter = 0
	    		def getIntLiteral = { counter+=1; IntLiteral(counter) }
	    		
	    		val list0 = () => CaseClass(nilClass, Nil)
	    		val list1 = () => CaseClass(consClass, getIntLiteral :: list0() :: Nil)
	    		val list2 = () => CaseClass(consClass, getIntLiteral :: list1() :: Nil)
	    		val list3 = () => CaseClass(consClass, getIntLiteral :: list2() :: Nil)
	    		val list4 = () => CaseClass(consClass, getIntLiteral :: list3() :: Nil)
	    		
	    		val lists = List(list0, list1, list2, list3/*, list4*/)
	    		
	    		for(l1 <- lists; l2 <- lists)
		        yield Map(argumentIds(0) -> l1(), argumentIds(1) -> l2())
	    		
	      case _ =>
	        fail("Could not produce initial examples: Could not match hole type")
	        null
		    }
//      case _ =>
//        fail("Could not produce initial examples: could not extract program and hole")
//        null
//    }    
  }
  
}