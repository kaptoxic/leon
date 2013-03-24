package lesynth
package rules

import leon.purescala.Trees._
import leon.purescala.Common._
import leon.purescala.TypeTrees._
import leon.purescala.TreeOps._
import leon.purescala.Extractors._
import leon.purescala.Definitions._

import leon.synthesis.{ Rule, RuleInstantiation, SynthesisContext, Problem, Solution }

import InputExamples._

case object ConditionAbductionSynthesis extends Rule("Condition abduction synthesis.") {
  def instantiateOn(sctx: SynthesisContext, p: Problem): Traversable[RuleInstantiation]= {
    val solver = sctx.simpleSolver
		val program = sctx.program
		val reporter = sctx.reporter
				
		p.as match {
      case givenVariable :: Nil =>
        val desiredType = givenVariable.getType
        val holeFunDef = sctx.functionContext.get
        
        val synthesizer = new SynthesizerForRuleExamples(
          program, desiredType, holeFunDef,
          reporter = reporter,
          introduceExamples = introduceTwoListArgumentsExamples
        )
        
        synthesizer.synthesize match {
          case EmptyReport => None
          case FullReport(resFunDef, _) =>        
		        List(
		          RuleInstantiation.immediateSuccess(p, this,
		            Solution(resFunDef.getPrecondition, Set.empty, resFunDef.body.get)
		          )
		        )
        }
      case _ =>
        Nil
    }
    
  }
}
