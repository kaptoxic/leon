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

class RedBlackTreeTest extends FunSuite {

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
import scala.collection.immutable.Set

import leon.Utils._

object RedBlackTree { 
  sealed abstract class Color
  case class Red() extends Color
  case class Black() extends Color
 
  sealed abstract class Tree
  case class Empty() extends Tree
  case class Node(color: Color, left: Tree, value: Int, right: Tree) extends Tree

  def content(t : Tree) : Set[Int] = t match {
    case Empty() => Set.empty
    case Node(_, l, v, r) => content(l) ++ Set(v) ++ content(r)
  }

  def size(t : Tree) : Int = t match {
    case Empty() => 0
    case Node(_, l, v, r) => size(l) + 1 + size(r)
  }
  
  def isBlack(t: Tree) : Boolean = t match {
    case Empty() => true
    case Node(Black(),_,_,_) => true
    case _ => false
  }
  
  def balance(in: Tree): Tree =
    choose { (res: Tree) =>
      passes(
        Map[Tree, Tree](
//Node(Red(),Node(Black(),Empty(),1,Empty()),1,Empty()) -> Empty(),
//Node(Red(),Node(Red(),Empty(),1,Empty()),1,Empty()) -> Empty()
Node(Black(),Node(Red(),Node(Red(),Node(Black(),Empty(),1,Empty()),1,Node(Black(),Empty(),2,Empty())),2,Node(Black(),Empty(),3,Empty())),3,Node(Black(),Empty(),4,Empty())) ->
Node(Red(),Node(Black(),Node(Black(),Empty(),1,Empty()),1,Node(Black(),Empty(),2,Empty())),2,Node(Black(),Node(Black(),Empty(),3,Empty()),3,Node(Black(),Empty(),4,Empty()))),
Node(Black(),Node(Red(),Node(Black(),Empty(),1,Empty()),1,Node(Red(),Node(Black(),Empty(),2,Empty()),2,Node(Black(),Empty(),3,Empty()))),3,Node(Black(),Empty(),4,Empty())) ->
Node(Red(),Node(Black(),Node(Black(),Empty(),1,Empty()),1,Node(Black(),Empty(),2,Empty())),2,Node(Black(),Node(Black(),Empty(),3,Empty()),3,Node(Black(),Empty(),4,Empty())))
,
Node(Black(),Node(Black(),Empty(),1,Empty()),1,Node(Red(),Node(Red(),Node(Black(),Empty(),2,Empty()),2,Node(Black(),Empty(),3,Empty())),3,Node(Black(),Empty(),4,Empty())))
-> Node(Red(),Node(Black(),Node(Black(),Empty(),1,Empty()),1,Node(Black(),Empty(),2,Empty())),2,Node(Black(),Node(Black(),Empty(),3,Empty()),3,Node(Black(),Empty(),4,Empty()))),
Node(Black(),Node(Black(),Empty(),1,Empty()),1,Node(Red(),Node(Black(),Empty(),2,Empty()),2,Node(Red(),Node(Black(),Empty(),3,Empty()),3,Node(Black(),Empty(),4,Empty()))))
-> Node(Red(),Node(Black(),Node(Black(),Empty(),1,Empty()),1,Node(Black(),Empty(),2,Empty())),2,Node(Black(),Node(Black(),Empty(),3,Empty()),3,Node(Black(),Empty(),4,Empty())))
//,
//Node(Red(),Node(Black(),Empty(),1,Empty()),1,Node(Black(),Empty(),2,Empty()))
//-> Node(Red(),Node(Black(),Empty(),1,Empty()),1,Node(Black(),Empty(),2,Empty()))
////Node(Black(),Node(Black(),Empty(),1,Empty()),1,Node(Black(),Empty(),2,Empty())) -> Node(Black(),Node(Black(),Empty(),1,Empty()),1,Node(Black(),Empty(),2,Empty())),
        )
        ,
        in, res
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
