package lesynth
package rules

import leon.purescala.Trees._
import leon.purescala.Common._
import leon.purescala.TypeTrees._
import leon.purescala.TreeOps._
import leon.purescala.Extractors._
import leon.purescala.Definitions._
import leon.purescala.DataGen.findModels
import leon.synthesis._
import leon.solvers.{ Solver, TimeoutSolver }
import leon.evaluators.CodeGenEvaluator

import InputExamples._

case object ConditionAbductionSynthesisTwoPhase extends Rule("Condition abduction synthesis (two phase).") {
  def instantiateOn(sctx: SynthesisContext, p: Problem): Traversable[RuleInstantiation] = {

    p.xs match {
      case givenVariable :: Nil =>
        List(new RuleInstantiation(p, this, SolutionBuilder.none, "Condition abduction") {
          def apply(sctx: SynthesisContext): RuleApplicationResult = {
            try {
              val solver = new TimeoutSolver(sctx.solver, 1000L)
              val program = sctx.program
              val reporter = sctx.reporter

              val desiredType = givenVariable.getType
              val contextFunDef = sctx.functionContext.get
              val holeFunDef = new FunDef(FreshIdentifier(contextFunDef.id.name), givenVariable.getType,
                p.as.map(id => VarDecl(id, id.getType)))
//              val holeFunDef = new FunDef(contextFunDef.id, givenVariable.getType,
//                p.as.map(id => VarDecl(id, id.getType)))
              holeFunDef.precondition = Some(p.pc)      
              holeFunDef.postcondition = Some(p.phi)
              
              // this is just because synthesizer expects body
              holeFunDef.body = contextFunDef.body
              
              try {
                val freshResID = FreshIdentifier("result").setType(holeFunDef.returnType)
                val freshResVar = Variable(freshResID)
                
                val codeGenEval = new CodeGenEvaluator(sctx.context, sctx.program)
                def getInputExamples = 
                  getDataGenInputExamples(codeGenEval, p, 
                		100, 2000, Some(holeFunDef.args.map(_.id))
                	) _
                
                holeFunDef.postcondition = Some(replace(
                  Map(givenVariable.toVariable -> ResultVariable().setType(holeFunDef.returnType)), p.phi))
                holeFunDef.precondition = Some(p.pc)

                val newDefs = 
                  for (oldDef <- program.mainObject.defs) yield oldDef match {
	                  case `contextFunDef` => holeFunDef
	                  case _ => oldDef
                  }	       
                
                val synthesizer = new SynthesizerForRuleExamples(
                  solver, program.copy(mainObject = program.mainObject.copy(defs = newDefs)), desiredType, holeFunDef, p, sctx, freshResVar,
                  20, 2, 1,
                  reporter = reporter,
                  introduceExamples = getInputExamples,  
								  numberOfTestsInIteration = 50,
								  numberOfCheckInIteration = 5
							  )

                synthesizer.synthesize match {
                  case EmptyReport => RuleApplicationImpossible
                  case FullReport(resFunDef, _) =>
                    RuleSuccess(Solution(resFunDef.getPrecondition, Set.empty, resFunDef.body.get))
                }
              } catch {
                case e: Throwable =>
                  sctx.reporter.warning("Condition abduction crashed: " + e.getMessage)
                  e.printStackTrace
                  RuleApplicationImpossible
              } finally {
//                holeFunDef.postcondition = oldPostcondition
//                holeFunDef.precondition = oldPrecondition
              }
            }
          }
        })
      case _ =>
        throw new RuntimeException("should not")
        Nil
    }
  }
  
  def refineProblem(p: Problem, solver: Solver) = {

    val newAs = p.as.map {
      case oldId@IsTyped(id, AbstractClassType(cd)) =>

        val optCases = for (dcd <- cd.knownDescendents.sortBy(_.id.name)) yield dcd match {
          case ccd : CaseClassDef =>
            val toSat = And(p.pc, CaseClassInstanceOf(ccd, Variable(id)))
            	        
            val isImplied = solver.solveSAT(toSat) match {
              case (Some(false), _) => true
              case _ => false
            }
            
            println(isImplied)
            
            if (!isImplied) {
              Some(ccd)
            } else {
              None
            }
          case _ =>
            None
        }

        val cases = optCases.flatten
		
        println(id)
        println(cases)
        
        if (cases.size == 1) {
//          id.setType(CaseClassType(cases.head))
          FreshIdentifier(oldId.name).setType(CaseClassType(cases.head))
        } else oldId
          
      case id => id
    }
    
    p.copy(as = newAs)
  }
}
