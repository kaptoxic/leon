package lesynth
package rules

import leon.synthesis._
import InstantiateSynthesizer._

case object ExamplesAbductionSynthesisTwoPhase extends Rule("Examples abduction synthesis (two phase).") {
  def instantiateOn(sctx: SynthesisContext, p: Problem): Traversable[RuleInstantiation] = {
    instantiate(sctx, p)(this, instatiateDefaultSynthesizer(sctx, p).copy(generateInputExamples = false))
  }
}
