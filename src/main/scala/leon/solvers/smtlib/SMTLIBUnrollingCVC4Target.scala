/* Copyright 2009-2015 EPFL, Lausanne */

package leon
package solvers.smtlib

import leon.purescala.Common.FreshIdentifier
import leon.purescala.Expressions.Expr
import leon.purescala.Definitions.{ValDef, TypedFunDef}
import purescala.DefOps.typedTransitiveCallees
import leon.purescala.ExprOps.{variablesOf, matchToIfThenElse}
import smtlib.parser.Commands._
import smtlib.parser.Terms._

trait SMTLIBUnrollingCVC4Target extends SMTLIBCVC4Target {
  this: SMTLIBSolver =>

  private val typedFunDefExplorationLimit = 10000

  override def targetName = "2.5-cvc4"
  override def declareFunction(tfd: TypedFunDef): SSymbol = {
    if (tfd.params.isEmpty) {
      super[SMTLIBCVC4Target].declareFunction(tfd)
    } else {
      val (funs, exploredAll) = typedTransitiveCallees(Set(tfd), Some(typedFunDefExplorationLimit))
      if (!exploredAll) {
        reporter.warning(
          s"Did not manage to explore the space of typed functions trasitively called from ${tfd.id}. The solver may fail"
        )
      }

      val smtFunDecls = funs.toSeq.flatMap {
        case tfd if tfd.params.isEmpty =>
          // FIXME: Here we actually want to call super[SMTLIBCVC4Target].declareFunction(tfd),
          // but we inline it to work around a freakish compiler bug
          if (!functions.containsA(tfd)) {
            val id = if (tfd.tps.isEmpty) {
              tfd.id
            } else {
              FreshIdentifier(tfd.id.name)
            }
            sendCommand( DeclareFun(id2sym(id),Seq(),declareSort(tfd.returnType)) )
          }
          None
        case tfd if !functions.containsA(tfd) && tfd.params.nonEmpty =>
          val id = if (tfd.tps.isEmpty) {
            tfd.id
          } else {
            tfd.id.freshen
          }
          val sym = id2sym(id)
          functions +=(tfd, sym)
          Some(FunDec(
            sym,
            tfd.params map { p => SortedVar(id2sym(p.id), declareSort(p.getType)) },
            declareSort(tfd.returnType)
          ))
        case _ => None
      }
      val smtBodies = smtFunDecls map { case FunDec(sym, _, _) =>
        val tfd = functions.toA(sym)
        toSMT(matchToIfThenElse(tfd.body.get))(tfd.params.map { p =>
          (p.id, id2sym(p.id): Term)
        }.toMap)
      }

      if (smtFunDecls.nonEmpty) sendCommand(DefineFunsRec(smtFunDecls, smtBodies))
      functions.toB(tfd)
    }
  }

  // For this solver, we prefer the variables of assert() commands to be exist. quantified instead of free
  override def assertCnstr(expr: Expr): Unit = {
    val existentials = variablesOf(expr).toSeq

    val term = if (existentials.isEmpty) toSMT(expr)(Map()) else {
      val es = existentials map { id =>
        SortedVar(id2sym(id), declareSort(id.getType))
      }
      Exists(
        es.head,
        es.tail,
        toSMT(expr)(existentials.map { id => id -> (id2sym(id): Term)}.toMap)
      )
    }
    sendCommand(Assert(term))
  }

}
