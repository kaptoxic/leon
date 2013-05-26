package lesynth
package rules

import leon.synthesis._
import InstantiateSynthesizer._

case object ConditionAbductionSynthesisTwoPhase extends Rule("Condition abduction synthesis (two phase).") {
  def instantiateOn(sctx: SynthesisContext, p: Problem): Traversable[RuleInstantiation] = {
    instantiate(sctx, p)(this)
  }
}
