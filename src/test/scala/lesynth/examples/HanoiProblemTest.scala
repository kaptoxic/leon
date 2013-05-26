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

class HanoiProblemTest extends FunSuite {

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
	    """import leon.Utils._

		object HanoiObject {
		  
		  sealed abstract class Peg
		  case object Src extends Peg
		  case object Aux extends Peg
		  case object Dst extends Peg
		  
		  abstract class State
		  case object Initial extends State
		  case class Move(disks: Int, src: Peg, dst: Peg, state: State) extends State
		  
		  case class Hanoi(disks: Int, src: Peg, aux: Peg, dst: Peg, state: State)
		  
  		  def dec(value: Int) = value - 1
  	    
		  def isZero(value: Int) = value == 0
  	    
		  def solve(hanoi: Hanoi): State = choose {
		    (res: State) =>
		      passes(
			    Map[Hanoi, State](
		          Hanoi(0, Src, Aux, Dst, Initial) -> Move(0, Src, Dst, Initial),
		          Hanoi(1, Src, Aux, Dst, Initial) ->
				    Move(0, Aux, Dst,
		    		  Move(1, Src, Dst,
					    Move(0, Src, Aux, Initial)
					  )
		    		),
		          Hanoi(2, Src, Aux, Dst, Initial) -> 
					  Move(0, Src, Dst, 
						Move(1, Aux, Dst,
						  Move(0, Aux, Src,
							Move(2, Src, Dst, 
							  Move(0, Dst, Aux,
								Move(1, Src, Aux,
								  Move(0, Src, Dst, Initial)))))))
		        ),
		        hanoi, res
			  )
		  }
		}
	    """
    )) {
  	  val predicate = p.phi
  	  val arguments = f.args.map( _.id ).toSet
  	  expect(true) {
  	    for (ruleInst <- ExamplesAbductionSynthesisTwoPhase.instantiateOn(sctx, p)) {
  	      ruleInst.apply(sctx)
  	    }
  	  }
  	}
  }

}
