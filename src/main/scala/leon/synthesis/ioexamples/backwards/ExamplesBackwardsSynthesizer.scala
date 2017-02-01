package leon
package synthesis
package ioexamples
package backwards

import purescala._
import Expressions._
import Common._
import Types._
import ExprOps._
import Extractors._
import Definitions._

import solvers._
import evaluators.CodeGenEvaluator

import leon.utils.Timer

case object ExamplesBackwardsSynthesizer extends
  Rule("Input/output example synthesis, backwards discovery method (two phase).") {

  def instantiateOn(implicit sctx: SearchContext, p: Problem): Traversable[RuleInstantiation] = {

    p.xs match {
      case givenVariable :: Nil =>
        new RuleInstantiation(
          "Backwards discovery (IO examples)"
//          SolutionBuilder.none
        ) (p, this) {
          def apply(sctx: SearchContext): RuleApplication = {
            try {
              val program = sctx.program
              val reporter = sctx.reporter

              val desiredType = givenVariable.getType

					    val mappings = ExamplesExtraction.extract(problem.phi)
					    
					    val ioExamples = ExamplesExtraction.transformMappings(mappings)
					    
					    ioExamples match {
//					      case Some(((inId, outId), exampleList)) =>
//					        val synthesizer = new Synthesizer
//					        
//					        synthesizer.synthesize(exampleList) match {
//					          case Some((body, newFun)) =>					            
//					            RuleSuccess(
//						          	Solution(BooleanLiteral(true), 
//	                        Set(newFun),
//	                        body
//	                      )
//                      )
//					          case None =>
//					          	sctx.reporter.warning("IO synthesis performed but failed")
//					            RuleApplicationImpossible					            
//					        }
					        
					      case _ =>
                  sctx.reporter.warning("Bad form of passes for IO synthesis")
					        RuleFailed()
					    }
            }
          }
        } :: Nil
      case _ =>
        Nil
    }

  }

}
