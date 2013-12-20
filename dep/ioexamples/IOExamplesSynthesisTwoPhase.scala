package leon.synthesis
package ioexamples

import leon.purescala.Trees._
import leon.purescala.Common._
import leon.purescala.TypeTrees._
import leon.purescala.TreeOps._
import leon.purescala.Extractors._
import leon.purescala.Definitions._
import leon.synthesis._
import leon.solvers._
import leon.evaluators.CodeGenEvaluator

import leon.StopwatchCollections

case object IOExamplesSynthesisTwoPhase extends Rule("Input/output example synthesis (two phase).") {

  def instantiateOn(sctx: SynthesisContext, p: Problem): Traversable[RuleInstantiation] = {

    p.xs match {
      case givenVariable :: Nil =>
        List(new RuleInstantiation(p, this, SolutionBuilder.none, "Condition abduction") {
          def apply(sctx: SynthesisContext): RuleApplicationResult = {
            try {
              val program = sctx.program
              val reporter = sctx.reporter

              val desiredType = givenVariable.getType

					    val mappings = ExamplesExtraction.extract(problem.phi)
					    
					    val ioExamples = ExamplesExtraction.transformMappings(mappings)
					    
					    ioExamples match {
					      case Some(((inId, outId), exampleList)) =>
					        val synthesizer = new Synthesizer
					        
					        synthesizer.synthesize(exampleList) match {
					          case Some((body, newFun)) =>					            
					            RuleSuccess(
						          	Solution(BooleanLiteral(true), 
	                        Set(newFun),
	                        body
	                      )
                      )
					          case None =>
					          	sctx.reporter.warning("IO synthesis performed but failed")
					            RuleApplicationImpossible					            
					        }
					        
					      case _ =>
                  sctx.reporter.warning("Bad form of passes for IO synthesis")
					        RuleApplicationImpossible
					    }
            }
          }
        })
      case _ =>
        Nil
    }

  }

}
