package leon
package synthesis.ioexamples

import scala.collection.mutable.{ Map => MMap, TreeSet }

import purescala._
import Types._
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
  
  def filterOut(t: Expr) =
    t match {
      case NilList(_) => true
      case CaseClass(_, args) if args.size == 0 => true
      case _ => false
    }
  
  // compute fragments for each given example pair
  def constructFragments(examples: List[IO], inputVariables: List[Variable]): List[Expr] = {
    for ((ie, oe) <- examples) yield {
      // all subexpressions of input
      val subexprMaps = u.mapOfSubexpressionsToPathFunctions(ie)

      // if we have an integer input var, do -1, -2, ...
      // TODO scan output example for this
      val existsInt = inputVariables.exists(_.getType == Int32Type)
      info("existsInt = " + existsInt)

      val modifiedMaps =
        if (existsInt) {
          // we do (not) need to substitute for atoms
          subexprMaps/*.map(_.filterNot( e => filterOut(e._1) ))*/ map {
            subExprMap =>
              subExprMap ++ (
                for ((IntLiteral(v), path) <- subExprMap)
                  yield
                    Map(
                      (IntLiteral(v - 1): Expr) -> { (x: Expr) => path(Minus(x, IntLiteral(1))) },
                      (IntLiteral(v - 2): Expr) -> {
                        (x: Expr) => path(Minus(Minus(x, IntLiteral(1)), IntLiteral(1))) }
                    )
              ).flatten
          } 
        } else subexprMaps
      
      assert(modifiedMaps.size == inputVariables.size)
      val mapped = for ((modifiedMap, inputVariable) <- (modifiedMaps zip inputVariables)) yield {
        info(s"Applying $inputVariable")
        modifiedMap.mapValues(_(inputVariable))
      }
      info("Maps:")
      info(mapped.mkString("\n"))
      
      // map output examples and make input variable in place of placeholder
      constructFragment(oe, mapped)
    }
  }
  
  
  def constructFragments(examples: List[IO], inputVariables: List[Common.Identifier])
    (implicit dummy: DummyImplicit): List[Expr] =
    constructFragments(examples, inputVariables.map(_.toVariable))
  
  def constructFragment(tree: Expr, maps: Iterable[Map[Expr, Expr]]) = {
        
    val cumulativeMap = 
      (Map.empty[Expr, Expr] /: maps) {
        case (current, map) => current ++ map
      }.mapValues(Some(_))

    info(s"cummulativeMap $cumulativeMap")
    
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
  
  // check if there are ambigous examples, in terms of fragments (can be explained with either variable,
  //  with same size of expression
  // TODO: refactor this since it is almost identical to prvious function
  def checkAmbiguity(examples: List[IO], inputVariables: List[Variable]) = {
    entering("checkAmbiguity")
    
//    def constructFragmentsInner(examples: List[IO], inputVariables2: List[Common.Identifier])(
//      implicit dummy: DummyImplicit): List[Expr] = {
//      constructFragments(examples, inputVariables2.map(_.toVariable))
//    }
    
    def constructFragmentInner(tree: Expr, maps: Iterable[Map[Expr, Expr]]) = {
          
      val cumulativeMap = 
        (Map.empty[Expr, Set[Expr]] /: maps) {
          case (current, map) =>
            val mapOfSets = map.mapValues(Set(_))
            mapOfSets.map({ case (k, v) => (k, v union current.getOrElse(k, Set[Expr]())) })
        }
  
      info(s"cummulativeMap $cumulativeMap")
      
      var isAmbigous = false
      // for each detected subexpression (the largest one first) check
      val mapped =
        ExprOps.preMapWithContext({ (e: Expr, done: Boolean) =>
          // check done to stop recursing
          if (done)
            (None, true)
          else if (cumulativeMap contains e) {
            val alternatives = cumulativeMap(e)
            val (ambigous, nonAmbigous) =
              // TODO kinda hardcoded
              // XXX -- hack -- make sort do equivalence classes
              alternatives.partition(inputVariables contains _)
            info("(ambigous, nonAmbigous)" + (ambigous, nonAmbigous))
            if (ambigous.size >= 2) {
              isAmbigous = true
              (Some(ambigous.head), true)
            }
              (Some(Hole(e.getType, alternatives.toSeq): Expr), true)
          }
          else (None, false)
        }, false)(tree, false)
        
      (mapped, isAmbigous)
    }
    
    val mapped =
      for ((ie, oe) <- examples) yield {
        // all subexpressions of input
        val subexprMaps = u.mapOfSubexpressionsToPathFunctions(ie)
  
        // if we have an integer input var, do -1, -2, ...
        // TODO scan output example for this
        val existsInt = inputVariables.exists(_.getType == Int32Type)
        info("existsInt = " + existsInt)
  
        val modifiedMaps =
          if (existsInt) {
            // we do (not) need to substitute for atoms
            subexprMaps/*.map(_.filterNot( e => filterOut(e._1) ))*/ map {
              subExprMap =>
                subExprMap ++ (
                  for ((IntLiteral(v), path) <- subExprMap)
                    yield
                      Map(
                        (IntLiteral(v - 1): Expr) -> { (x: Expr) => path(Minus(x, IntLiteral(1))) },
                        (IntLiteral(v - 2): Expr) -> {
                          (x: Expr) => path(Minus(Minus(x, IntLiteral(1)), IntLiteral(1))) }
                      )
                ).flatten
            } 
          } else subexprMaps
        
        assert(modifiedMaps.size == inputVariables.size)
        val mapped = for ((modifiedMap, inputVariable) <- (modifiedMaps zip inputVariables)) yield {
          info(s"Applying $inputVariable")
          modifiedMap.mapValues(_(inputVariable))
        }
        info("Maps:")
        info(mapped.mkString("\n"))
        
        // map output examples and make input variable in place of placeholder
        constructFragmentInner(oe, mapped)
      }
    
//    val (ambigous, nonAmbigous) = mapped.partition(_._2)
//    
//    (ambigous.map(_._1), nonAmbigous.map(_._1))  
    mapped
  }

}