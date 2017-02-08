package leon
package synthesis.ioexamples

import scala.collection.mutable.{ Map => MMap, TreeSet }

import purescala._
import Expressions._

import utils.logging.HasLogger

/*
 * Computes the decomposed input set
 * 
 * This algorithm corresponds to P2 for matching output because it tries to
 * match the topmost expression, if it cannot, it proceeds recursively (with searchAndReplace)
 */
object Fragmenter extends HasLogger {
  
  // same is in the Synthesizer
  type IO = (List[Expr], Expr)
  
  val u = Util
  import ExprOps._
  import Extractors._
  
  // compute fragments for each given example pair
  def constructFragments(examples: List[IO], inputVariables: List[Variable]): List[Expr] = {
    for ((ie, oe) <- examples) yield {
      // all subexpressions of input
      val subexprMaps = u.mapOfSubexpressions(ie)
      // we do not need to substitute for nil
      val modifiedMaps = subexprMaps.map(_.filterNot( _._1.isInstanceOf[NilList] ))
      
      val mapped = for ((modifiedMap, inputVariable) <- (modifiedMaps zip inputVariables)) yield
        modifiedMap.mapValues(_(inputVariable))
      
      // map output examples and make input variable in place of placeholder
      constructFragment(oe, mapped)
    }
  }
  
  def constructFragment(tree: Expr, maps: Iterable[Map[Expr, Expr]]) = {
        
    val cumulativeMap = 
      (Map.empty[Expr, Expr] /: maps) {
        case (current, map) => current ++ map
      }.mapValues(Some(_))
    
    // for each detected subexpression (the largest one first) substitute
//    postMap({ e => modifiedMap.getOrElse(e, None) }, false)(tree)
    preMapWithContext({ (e: Expr, done: Boolean) =>
      // check done to stop recursing
      if (done)
        (None, true)
      else if (cumulativeMap contains e)
        (cumulativeMap(e), true)
      else (None, false)
    }, false)(tree, false)
  }

}