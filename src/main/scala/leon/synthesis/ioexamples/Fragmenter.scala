package leon
package synthesis.ioexamples

import scala.collection.mutable.{ Map => MMap, TreeSet }

import purescala.Trees._
import purescala._

import insynth.util.logging.HasLogger

object Fragmenter extends HasLogger {
  
  val u = Util
  import TreeOps._
  import Extractors._
  
  def constructFragment(tree: Expr, map: Map[Expr, Expr]) = {
        
//    simplePostTransform(_ match {
//      case nl: NilList => nl
//      case c: Cons => c
//      case car: Car => car
//      case cdr: Cdr => cdr
//      // variable supported as atoms
//      case v: Variable => v
//      case _ => throw new RuntimeException("Not supported")
//    } )(tree)
    val modifiedMap = map.mapValues(Some(_))
    
    searchAndReplace({ e => modifiedMap.getOrElse(e, None) }, false)(tree)
  }
  
  def constructFragments(examples: List[(Expr, Expr)], inputVariable: Variable) = {
    
    for ((ie, oe) <- examples) yield {
      val subexprMap = u.mapOfSubexpressions(ie)
      // we do not need to substitute for nil
    	val modifiedMap = subexprMap.filterNot( _._1.isInstanceOf[NilList]  )
    	    	
      constructFragment(oe, modifiedMap.mapValues(_(inputVariable)))
    }
    
  }

}