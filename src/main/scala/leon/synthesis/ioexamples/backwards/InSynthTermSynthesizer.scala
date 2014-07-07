package leon
package synthesis
package ioexamples.backwards

import purescala.Trees._
import purescala.TypeTrees._
import purescala.Definitions.{ FunDef, Program }
import synthesis.{ Problem, SynthesisContext }

import insynth.engine._
import condabd.insynth._
import condabd.insynth.leon.loader._
import condabd.insynth.leon.{ LeonDeclaration => Declaration, _ }
import condabd.insynth.reconstruction.Output

class InSynthTermSynthesizer(program: Program, problem: Problem,
  numOfSnippetsToSynthesize: Int = 20) extends ( TypeTree => List[Expr] ) {
  
  def apply(tpe: TypeTree) =
    if (tpe == BooleanType) {
      val stream = inSynthBoolean.getExpressions(getCurrentBuilder)
      stream.distinct.take(numOfSnippetsToSynthesize).map(_.getSnippet).toList
    } else
      throw new IllegalArgumentException("Cannot synthesize for: " + tpe)

  var allDeclarations: List[Declaration] = _

  val loader = new LeonLoader(program, problem.as, false)
  val inSynthBoolean = new InSynth(loader, BooleanType, true)
  
  // save all declarations seen
  allDeclarations = inSynthBoolean.getAllDeclarations

  def getCurrentBuilder = new InitialEnvironmentBuilder(allDeclarations)
}