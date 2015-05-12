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

class BinarySearchTreeUnion extends FunSuite {

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

import leon.Annotations._
import leon.Utils._

object BinarySearchTree {
  sealed abstract class Tree
  case class Node(left: Tree, value: Int, right: Tree) extends Tree
  case class Leaf() extends Tree

  def contents(tree: Tree): Set[Int] = tree match {
    case Leaf() => Set.empty[Int]
    case Node(l, v, r) => contents(l) ++ Set(v) ++ contents(r)
  }
 
  case class Pair(t1: Tree, t2: Tree)

  def insert(tree: Tree, value: Int): Node = {
    tree match {
      case Leaf() => Node(Leaf(), value, Leaf())
      case n @ Node(l, v, r) => if (v < value) {
        Node(l, v, insert(r, value))
      } else if (v > value) {
        Node(insert(l, value), v, r)
      } else {
        n
      }
    }
  }
  
  def union(p: Pair): Tree = choose {
    (res: Tree) =>
      passes(
      Map[Pair, Tree](
          Pair(Leaf(), Node(Leaf(), 5, Leaf())) -> Node(Leaf(), 5, Leaf()),
          Pair(Node(Leaf(), 4, Leaf()), Node(Leaf(), 5, Leaf())) ->
            Node(Node(Leaf(), 4, Leaf()), 5, Leaf()),
          Pair(Node(Node(Leaf(), 3, Leaf()), 4, Leaf()), Node(Leaf(), 5, Leaf())) ->
            Node(Node(Leaf(),3,Node(Leaf(),4,Leaf())),5,Leaf()),
          Pair(Node(Leaf(), 4, Node(Leaf(), 5, Leaf())), Node(Leaf(), 8, Leaf())) ->
            Node(Node(Node(Leaf(),4,Leaf()),5,Leaf()),8,Leaf()),
          Pair(Node(Node(Leaf(), 3, Leaf()), 4, Node(Leaf(), 5, Leaf())), Node(Leaf(), 8, Leaf())) ->
            Node(Node(Node(Leaf(),3,Node(Leaf(),4,Leaf())),5,Leaf()),8,Leaf())
        ),
        p, res
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
