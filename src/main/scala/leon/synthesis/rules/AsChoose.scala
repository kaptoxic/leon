/* Copyright 2009-2013 EPFL, Lausanne */

package leon
package synthesis
package rules

import solvers.TimeoutSolver
import purescala.Trees._
import purescala.TypeTrees._
import purescala.TreeOps._
import purescala.Extractors._

case object AsChoose extends Rule("As Choose") {
  def instantiateOn(sctx: SynthesisContext, p: Problem): Traversable[RuleInstantiation] = {
      Some(RuleInstantiation.immediateSuccess(p, this, Solution.choose(p)))
  }
}

