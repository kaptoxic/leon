package lesynth
package examples

import leon.purescala.Trees._
import leon.purescala.TreeOps._
import leon.purescala.Definitions._
import leon.purescala.Common.Identifier

import insynth.util.logging.HasLogger

object ExamplesExtraction extends HasLogger {

  def extract(predicate: Expr, arguments: Set[Identifier]): Seq[InputOutputExample] = {
    info("extracting examples from predicate: " + predicate)

    def extractMapping(e: Expr, otherSeq: Seq[InputOutputExample]): Seq[InputOutputExample] = e match {
      case passAnd @ And(exprs) if exprs.forall(
        _ match {
          case Implies(Equals(f, rin), Equals(rout, t)) if {
            fine("allIdentifiers(rin): " + allIdentifiers(rin) + "arguments: " + arguments)
            allIdentifiers(rin) == arguments
          } => true
          case _ => false
        }) => {
        fine("found needed And: " + passAnd)
        (for (expr <- exprs)
          yield expr match {
	          case Implies(Equals(f, Variable(id)), Equals(rout, t)) => InputOutputExample(Map(id -> f), t)
	          case Implies(Equals(exprTuple@Tuple(valExpr), idType@Tuple(idExprs)), Equals(rout, t))
	          	if valExpr.size == arguments.size && idExprs.forall(_.isInstanceOf[Variable]) => {
	            val idMap =
	              (for ((idExpr, valExpr) <- idExprs zip valExpr;
            		  val id = idExpr.asInstanceOf[Variable].id) yield	                
	                (id, valExpr)).toMap
	            InputOutputExample(idMap, t)	            
	          }
        	}) ++ otherSeq
      }
      case _ =>
        otherSeq
    }

    // Look for passes()
    treeCatamorphism(
      x => Nil,
      (l1: Seq[InputOutputExample], l2: Seq[InputOutputExample]) => l1 ++ l2,
      extractMapping,
      predicate)
  }
  
  implicit def pairToInputOutputExample(pair: (Map[Identifier, Expr], Expr)) =
    InputOutputExample(pair._1, pair._2)

}