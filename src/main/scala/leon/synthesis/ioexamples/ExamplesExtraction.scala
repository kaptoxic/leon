package leon
package synthesis
package ioexamples

import leon.purescala.Trees._
import leon.purescala.TreeOps._
import leon.purescala.Definitions._
import leon.purescala.Common.Identifier

import insynth.util.logging.HasLogger

object ExamplesExtraction extends HasLogger {
  
  type InputOutputExample = ((Identifier, Expr), (Identifier, Expr))
  
  def extract(predicate: Expr): Seq[InputOutputExample] = {
    info("extracting examples from predicate: " + predicate)

    def extractMapping(e: Expr, otherSeq: Seq[InputOutputExample]): Seq[InputOutputExample] = e match {
      case passAnd @ And(exprs) if exprs.forall(
        _ match {
          case Implies(Equals(inE, _: Variable), Equals(_: Variable, outE)) 
//          if {
//            fine("allIdentifiers(rin): " + variablesOf(rin) + "arguments: " + arguments)
//            variablesOf(rin) == arguments
//          }
          	=> true
          case _ => false
        }) => {
        fine("found needed And: " + passAnd)
        val mappingsInThisAnd =
	        (for (expr <- exprs)
	          yield expr match {
		          case Implies(Equals(f, Variable(idIn)), Equals(Variable(idOut), t)) => ((idIn -> f), (idOut -> t))
	//	          case Implies(Equals(exprTuple@Tuple(valExpr), idType@Tuple(idExprs)), Equals(rout, t))
	//	          	if valExpr.size == arguments.size && idExprs.forall(_.isInstanceOf[Variable]) => {
	//	            val idMap =
	//	              (for ((idExpr, valExpr) <- idExprs zip valExpr;
	//            		  val id = idExpr.asInstanceOf[Variable].id) yield	                
	//	                (id, valExpr)).toMap
	//	            InputOutputExample(idMap, t)	            
	//	          }
	        	})
	        	
        mappingsInThisAnd	++ otherSeq
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
  
  def transformMappings(mappings: Seq[InputOutputExample]) = {
    val idsAndExamples =
      ((Set[(Identifier, Identifier)](), Set[(Expr, Expr)]()) /: mappings) {
      	case ((setIds, setIOs), ((inId, inE), (outId, outE))) =>
      	  ( setIds + ((inId, outId)), setIOs + ((inE, outE)) )
    	  case _ =>
    	    (Set.empty, Set.empty)
    	}
    
    if (idsAndExamples._1.size == 1) {
      Some((idsAndExamples._1.head, idsAndExamples._2.toList))
    } else None
  }
  
//  implicit def pairToInputOutputExample(pair: (Map[Identifier, Expr], Expr)) =
//    InputOutputExample(pair._1, pair._2)

}