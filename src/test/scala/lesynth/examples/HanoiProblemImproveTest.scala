package lesynth
package examples

import java.io.{BufferedWriter, FileWriter, File}

import leon._
import leon.purescala.Definitions._
import leon.purescala.Trees._
import leon.purescala.TreeOps._
import leon.solvers.z3._
import leon.solvers.Solver
import leon.synthesis._
import leon.synthesis.utils._

import lesynth.rules._

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers._

class HanoiProblemImproveTest extends FunSuite {

  def forProgram(content: String): Iterable[(SynthesisContext, FunDef, Problem)] = {

    val ctx = LeonContext(
      settings = Settings(
        synthesis = true,
        xlang     = false,
        verify    = false
      ),
      files = List(),
      reporter = new SilentReporter
    )

    val opts = SynthesisOptions()

    val pipeline = leon.plugin.TemporaryInputPhase andThen leon.plugin.ExtractionPhase andThen SynthesisProblemExtractionPhase

    val (program, results) = pipeline.run(ctx)((content, Nil))

    val solver = new FairZ3Solver(ctx)
    solver.setProgram(program)

    val simpleSolver = new UninterpretedZ3Solver(ctx)
    simpleSolver.setProgram(program)

    for ((f, ps) <- results; p <- ps) 
    	yield {
        val sctx = SynthesisContext(ctx,
                                    opts,
                                    Some(f),
                                    program,
                                    solver,
                                    simpleSolver,
                                    new DefaultReporter,
                                    new java.util.concurrent.atomic.AtomicBoolean)

        (sctx, f, p)
    	}
  }

  test("simple one-argument passes") {
    
  	for ((sctx, f, p) <- forProgram(
	    """
		import leon.Utils._

object HanoiImprovedObject {
  
  sealed abstract class List
  case class Cons(head: Move, tail: List) extends List
  case object Nil extends List
  
  def concat(l1: List, l2: List) : List = l1 match {
    case Nil => l2
    case Cons(h, t) =>
      Cons(h, concat(t, l2))
  } 
    
  sealed abstract class Peg
  case object Src extends Peg
  case object Aux extends Peg
  case object Dst extends Peg
  
  case class Move(src: Peg, dst: Peg)
  
  case class Hanoi(disks: Int, src: Peg, aux: Peg, dst: Peg)
  
  def dec(value: Int) = value - 1
  
  def isZero(value: Int) = value == 0
  
  def solve(hanoi: Hanoi) = choose {
    (res: List) =>
      passes(
        Map[Hanoi, List]( (Hanoi(0, Src, Aux, Dst) -> Cons(Move(Src, Dst), Nil)),
    (Hanoi(1, Src, Aux, Dst) ->
      Cons(Move(Aux, Dst),
        Cons(Move(Src, Dst),
          Cons(Move(Src, Aux), Nil)))
    ),
    (Hanoi(2, Src, Aux, Dst) -> 
      Cons(Move(Src, Dst), 
      Cons(Move(Aux, Dst),
      Cons(Move(Aux, Src),
      Cons(Move(Src, Dst), 
      Cons(Move(Dst, Aux),
      Cons(Move(Src, Aux),
      Cons(Move(Src, Dst),
        Nil)))))))
    ) ),
        hanoi, res
      )
  }
    
}
	    """
    )) {
  	  val predicate = p.phi
  	  val arguments = f.args.map( _.id ).toSet
  	  expect(true) {
  	    for (ruleInst <- ExamplesAbductionSynthesisTwoPhaseImprove.instantiateOn(sctx, p)) {
  	      ruleInst.apply(sctx)
  	    }
  	  }
  	}
  }

}
