package lesynth
package examples

import leon._
import leon.purescala.Definitions._
import leon.purescala.Trees._
import leon.purescala.TreeOps._
import leon.solvers.z3._
import leon.solvers.Solver
import leon.synthesis._
import leon.synthesis.utils._

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers._

import java.io.{BufferedWriter, FileWriter, File}

class ExamplesExtractionTest extends FunSuite {

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

				object TestObject {
				  				  
				  abstract class A
				  case object B extends A
				  case class C(a: Int, b: Int) extends A
				  
				  def testFun(in: A) = choose {
				    (res: C) =>
				      passes(
					    Map[A, C](
				          B -> C(1, 2),
				          C(1, 0) -> C(1, 1)
				    		),
			        in, res
					  )
				  }
  			}
	    """
    )) {
  	  val predicate = p.phi
  	  val arguments = f.args.map( _.id ).toSet
  	  expect(true) {
  	    ExamplesExtraction.extract(predicate, arguments).toSet
  	  }
  	}
  }

}
