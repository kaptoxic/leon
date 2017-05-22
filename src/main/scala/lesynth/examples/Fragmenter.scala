package lesynth
package examples

import scala.collection.mutable.{ Map => MMap }

import leon._
import purescala.Trees._
import purescala._

import lesynth.examples._

object Fragmenter {
  
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
    
//    for ((ie, oe) <- examples) yield {
//      val subexprMap = mapOfSubexpressions(ie)
//      // we do not need to substitute for nil
//    	val modifiedMap = subexprMap.filterNot( _._1.isInstanceOf[NilList]  )
//    	    	
//      constructFragment(oe, modifiedMap.mapValues(_(inputVariable)))
//    }
    
  }
  
//  def mapOfSubexpressionsFunctions(ex: Expr): Map[Expr, (Expr => Expr)] = {
//    var map = MMap[Expr, (Expr => Expr)]()
//    
//    def transform(tree: Expr, ctx: Expr => Expr): Unit = tree match {
//      case nl@Leaf => map += nl -> ctx
//      case cons@Node(children) =>
//        map += cons -> ctx
//        for ((c, i) <- children.zipWithIndex)
//          transform(c, se => Accessor(ctx(se), DeBruijnIndex(i)))
//      // variable supported as atoms
//      case v: Variable =>
//        map += v -> ctx
//      case _ => throw new RuntimeException("This subtree is not supported")
//    }
//    
//    transform(ex, identity)
//    
//    map.toMap
//  }
  
  def mapOfSubexpressions(ex: Expr): Map[Expr, Expr] = {
    var map = MMap[Expr, Expr]()
    
    def add(from: Expr, to: Expr) {
      // TODO here we check if we have larger value for that key
      if (!map.contains(from))
        map += from -> to
    }
    
    def transform(tree: Expr, ctx: Expr): Unit = tree match {
      case leaf@CaseClass(_, Nil) =>
        add(leaf, ctx)
      case node@CaseClass(classDef, children) =>
        add(node, ctx)
        for ((c, i) <- children.zipWithIndex)
          transform(c, CaseClassSelector(classDef, ctx, classDef.fields(i).id))
      // variable supported as atoms
//      case v: Variable =>
//        map += v -> ctx
//      case _ =>
//        throw new RuntimeException("This subtree is not supported: " + tree)
      case e =>
        add(e, ctx)
    }
    
    transform(ex, ResultVariable())
    
    map.toMap
  }

}