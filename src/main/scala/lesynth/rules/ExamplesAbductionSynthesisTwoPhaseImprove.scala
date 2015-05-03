package lesynth
package rules

import leon.synthesis._
import InstantiateSynthesizer._

case object ExamplesAbductionSynthesisTwoPhaseImprove extends Rule("Examples abduction synthesis (two phase, improve).") {
  def instantiateOn(sctx: SynthesisContext, p: Problem): Traversable[RuleInstantiation] = {
    instantiate(sctx, p)(this, instatiateDefaultSynthesizer(sctx, p).copy(generateInputExamples = false))
  }
}
