package lesynth

import leon._
import leon.purescala.Definitions._
import leon.purescala.Trees._
import leon.purescala.TreeOps._
import leon.solvers.z3._
import leon.solvers.Solver
import leon.synthesis._
import leon.synthesis.utils._

import insynth.InSynth
import insynth.leon.loader.LeonLoader

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers._

import java.io.{BufferedWriter, FileWriter, File}

class EnumeratedExpressionsTest extends FunSuite {

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

  test("enumerate until hit the expression") {
    
  	for ((sctx, funDef, problem) <- forProgram("""
  	    import leon.Utils._

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
  			(res: State) => true
		  }
		}
	    """
    )) {
  	  val predicate = problem.phi
  	  val arguments = funDef.args.map( _.id ).toSet

  	  expect(1) { problem.xs.size }
  	  val resultVariable = problem.xs.head
  	  val hole = Hole(resultVariable.getType)
      // create new insynth object
      val loader = new LeonLoader(sctx.program, hole, problem.as, false)
      val inSynth = new InSynth(loader, true)
      // save all declarations seen
      val allDeclarations = inSynth.getCurrentBuilder.getAllDeclarations
      
      val maxNum = 1000000
      
      for ((expr, ind) <- inSynth.getExpressions.take(maxNum).map(_.snippet).zipWithIndex) {
        if ("""	| solve(Hanoi(dec(hanoi.disks), hanoi.aux, hanoi.src, hanoi.dst,
            """
//        		| Move(dec(hanoi.disks), hanoi.src, hanoi.dst,
//        		| solve(Hanoi(dec(hanoi.disks), hanoi.src, hanoi.dst, hanoi.aux, hanoi.state)))))
            .trim.stripMargin.replaceAll("\\s", "")
        		== expr.toString.replaceAll("\\s", "")) {
          println("found the string, at position " + ind + " !!!!")
        }
      }
  	  
  	  println("ended search")
  	}
  }

}
