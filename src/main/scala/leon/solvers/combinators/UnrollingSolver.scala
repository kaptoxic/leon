/* Copyright 2009-2014 EPFL, Lausanne */

package leon
package solvers
package combinators

import purescala.Common._
import purescala.Definitions._
import purescala.Trees._
import purescala.TreeOps._
import purescala.TypeTrees._

import utils.Interruptible

import scala.collection.mutable.{Map=>MutableMap}

class UnrollingSolver(val context: LeonContext, underlyings: SolverFactory[IncrementalSolver]) 
        extends Solver with Interruptible {

  private var theConstraint : Option[Expr] = None
  private var theModel : Option[Map[Identifier,Expr]] = None

  val reporter = context.reporter

  private var stop: Boolean = false

  def name = "U:"+underlyings.name

  def free {}

  import context.reporter._

  def assertCnstr(expression : Expr) {
    if(!theConstraint.isEmpty) {
      fatalError("Multiple assertCnstr(...).")
    }
    theConstraint = Some(expression)
  }

  def check : Option[Boolean] = theConstraint.map { expr =>
    val solver = underlyings.getNewSolver

    val template = getTemplate(expr)

    val aVar : Identifier = template.activatingBool
    var allBlockers : Map[Identifier,Set[FunctionInvocation]] = Map.empty

    def unrollOneStep() : List[Expr] = {
      val blockersBefore = allBlockers

      var newClauses : List[Seq[Expr]] = Nil
      var newBlockers : Map[Identifier,Set[FunctionInvocation]] = Map.empty

      for(blocker <- allBlockers.keySet; fi @ FunctionInvocation(tfd, args) <- allBlockers(blocker)) {
        val tmpl = getTemplate(tfd)

        val (nc, nb) = tmpl.instantiate(blocker, args)
        newClauses = nc :: newClauses
        newBlockers = newBlockers ++ nb
        //reporter.debug("Unrolling behind "+fi+" ("+nc.size+")")
        //for (c <- nc) {
        //  reporter.debug("  . "+c)
        //}
      }

      allBlockers = newBlockers
      newClauses.flatten
    }

    val (nc, nb) = template.instantiate(aVar, template.tfd.params.map(a => Variable(a.id)))

    allBlockers = nb

    var unrollingCount : Int = 0
    var done : Boolean = false
    var result : Option[Boolean] = None

    solver.assertCnstr(Variable(aVar))
    solver.assertCnstr(And(nc))
    // We're now past the initial step.
    while(!done && !stop) {
      solver.push()
      reporter.debug(" - Searching with blocked literals")
      solver.assertCnstr(And(allBlockers.keySet.toSeq.map(id => Not(id.toVariable))))
      solver.check match {

        case Some(false) =>
          solver.pop(1)
          reporter.debug(" - Searching with unblocked literals")
          //val open = fullOpenExpr
          solver.check match {
            case Some(false) =>
              done = true
              result = Some(false)

            case r =>
              unrollingCount += 1
              val model = solver.getModel
              reporter.debug(" - Tentative model: "+model)
              reporter.debug(" - more unrollings")
              val newClauses = unrollOneStep()
              reporter.debug(s"   - ${newClauses.size} new clauses")
              //readLine()
              solver.assertCnstr(And(newClauses))
          }

        case Some(true) =>
          val model = solver.getModel
          done = true
          result = Some(true)
          theModel = Some(model)

        case None =>
          val model = solver.getModel
          done = true
          result = Some(true)
          theModel = Some(model)
      }
    }
    solver.free
    result

  } getOrElse {
    Some(true)
  }

  def getModel : Map[Identifier,Expr] = {
    val vs : Set[Identifier] = theConstraint.map(variablesOf(_)).getOrElse(Set.empty)
    theModel.getOrElse(Map.empty).filter(p => vs(p._1))
  }

  override def interrupt(): Unit = {
    stop = true
  }

  override def recoverInterrupt(): Unit = {
    stop = false
  }

  private val tfdTemplateCache : MutableMap[TypedFunDef, FunctionTemplate] = MutableMap.empty
  private val exprTemplateCache : MutableMap[Expr, FunctionTemplate] = MutableMap.empty

  private def getTemplate(tfd: TypedFunDef): FunctionTemplate = {
    tfdTemplateCache.getOrElse(tfd, {
      val res = FunctionTemplate.mkTemplate(tfd, true)
      tfdTemplateCache += tfd -> res
      res
    })
  }

  private def getTemplate(body: Expr): FunctionTemplate = {
    exprTemplateCache.getOrElse(body, {
      val fakeFunDef = new FunDef(FreshIdentifier("fake", true), Nil, body.getType, variablesOf(body).toSeq.map(id => ValDef(id, id.getType)))
      fakeFunDef.body = Some(body)

      val res = FunctionTemplate.mkTemplate(fakeFunDef.typed, false)
      exprTemplateCache += body -> res
      res
    })
  }
}
