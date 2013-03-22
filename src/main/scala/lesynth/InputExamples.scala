package lesynth

import leon.{ Main => LeonMain, Reporter, DefaultReporter, SilentReporter, Settings, LeonContext }
import leon.solvers.{ Solver, TimeoutSolver }
import leon.solvers.z3.{ FairZ3Solver }
import leon.verification.AnalysisPhase
import leon.plugin.ExtractionPhase
import leon.purescala.TypeTrees.{ ClassType, CaseClassType }
import leon.purescala.Trees.{ IntLiteral, CaseClass }
import leon.purescala.Definitions.FunDef

import insynth.leon.loader.{ HoleExtractor, LeonLoader }

object InputExamples {
      
  def introduceOneListArgumentExamples(argumentIds: Seq[leon.purescala.Common.Identifier], loader: LeonLoader) = {
    
    // list type
    val ct = argumentIds(0).getType.asInstanceOf[ClassType]
    
		val setSubclasses = loader.directSubclassesMap(ct).map(_.asInstanceOf[CaseClassType].classDef)
		
		val (nilClassSet, consClassSet) = setSubclasses.partition(_.fieldsIds.size == 0)
		
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
  
  def introduceExamplesInsertionSortInsert(argumentIds: Seq[leon.purescala.Common.Identifier], loader: LeonLoader) = {
    
    // list type
    val ct = argumentIds(1).getType.asInstanceOf[ClassType]
    
		val setSubclasses = loader.directSubclassesMap(ct).map(_.asInstanceOf[CaseClassType].classDef)
		
		val (nilClassSet, consClassSet) = setSubclasses.partition(_.fieldsIds.size == 0)
		
		val nilClass = nilClassSet.head
		val consClass = consClassSet.head
		
		var counter = 10
		def getIntLiteral = { counter+=1; IntLiteral(counter) }
		val intLiteral = IntLiteral(5)
		
		val list0 = CaseClass(nilClass, Nil)
		val list1 = CaseClass(consClass, IntLiteral(10) :: list0 :: Nil)
		val list32 = CaseClass(consClass, IntLiteral(5) :: CaseClass(consClass, IntLiteral(7) :: list1 :: Nil) :: Nil)
		
		Map(argumentIds(0) -> IntLiteral(3), argumentIds(1) -> list32) ::
		Map(argumentIds(0) -> IntLiteral(2), argumentIds(1) -> list32) ::
		Map(argumentIds(0) -> IntLiteral(3), argumentIds(1) -> list0) ::
		Map(argumentIds(0) -> IntLiteral(2), argumentIds(1) -> list0) ::
		Map(argumentIds(0) -> IntLiteral(6), argumentIds(1) -> list32) ::
		Map(argumentIds(0) -> IntLiteral(9), argumentIds(1) -> list32) ::
		Nil    
  }
  
  def introduceOneListArgumentExamplesForMergeSort(argumentIds: Seq[leon.purescala.Common.Identifier], loader: LeonLoader) = {
    
    // list type
    val ct = argumentIds(0).getType.asInstanceOf[ClassType]
    
		val setSubclasses = loader.directSubclassesMap(ct).map(_.asInstanceOf[CaseClassType].classDef)
		
		val (nilClassSet, consClassSet) = setSubclasses.partition(_.fieldsIds.size == 0)
		
		val nilClass = nilClassSet.head
		val consClass = consClassSet.head
		
		var counter = 10
		def getIntLiteral = { counter+=1; IntLiteral(counter) }
		val intLiteral = IntLiteral(5)
		
		val list0 = CaseClass(nilClass, Nil)
		val list1 = CaseClass(consClass, IntLiteral(10) :: list0 :: Nil)
		val list2s = CaseClass(consClass, IntLiteral(5) :: list1 :: Nil)
		val list2u = CaseClass(consClass, IntLiteral(5) :: list1 :: Nil)
		val list3s = CaseClass(consClass, IntLiteral(5) :: list2s :: Nil)
		val list3u1 = CaseClass(consClass, IntLiteral(5) :: list2u :: Nil)
		val list3u2 = CaseClass(consClass, IntLiteral(15) :: list2s :: Nil)
		val list3u3 = CaseClass(consClass, IntLiteral(15) :: list2u :: Nil)
				
		for (list <- List(list0, list1, list2s, list2u, list3s, list3u1, list3u2, list3u3))
      yield Map(argumentIds(0) -> list)    
  }
  
  def introduceTwoListArgumentsExamples(argumentIds: Seq[leon.purescala.Common.Identifier], loader: LeonLoader) = {
        
		    loader.hole.getType match {
	      case ct: ClassType =>
	    		val setSubclasses = loader.directSubclassesMap(ct).map(_.asInstanceOf[CaseClassType].classDef)
	    		
	    		val (nilClassSet, consClassSet) = setSubclasses.partition(_.fieldsIds.size == 0)
	    		
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
	        null
		    }
  }
  
}