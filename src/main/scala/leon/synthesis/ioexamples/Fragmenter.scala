package leon
package synthesis.ioexamples

import scala.collection.mutable.{ Map => MMap, TreeSet }

import purescala.Trees._
import purescala._

import insynth.util.logging.HasLogger

/*
 * Computes the decomposed input set
 * 
 * This algorithm corresponds to P2 for matching output because it tries to
 * match the topmost expression, if it cannot, it proceeds recursively (with searchAndReplace)
 */
object Fragmenter extends HasLogger {
  
  val u = Util
  import TreeOps._
  import Extractors._
  
  // compute fragments for each given example pair
  def constructFragments(examples: List[IO], inputVariable: Variable): List[Expr] = {
    for ((ie, oe) <- examples) yield {
      // all subexpressions of input
      val subexprMap = u.mapOfSubexpressions(ie)
      // we do not need to substitute for nil
      val modifiedMap = subexprMap.filterNot( _._1.isInstanceOf[NilList]  )
      
      // map output examples and make input variable in place of placeholder
      constructFragment(oe, modifiedMap.mapValues(_(inputVariable)))
    }
  }
  
  def constructFragment(tree: Expr, map: Map[Expr, Expr]) = {
        
    val modifiedMap = map.mapValues(Some(_))
    
    // for each detected subexpression (the largest one first) substitute
    searchAndReplace({ e => modifiedMap.getOrElse(e, None) }, false)(tree)
  }

}