package leon
package synthesis.ioexamples

import scala.collection.mutable.{ Map => MMap }

import purescala.Trees._
import purescala._

object Fragmenter {
  
  val u = Util
  import TreeOps._
  import Extractors._

  def mapOfSubexpressions(ex: Expr): Map[Expr, Expr] = {
    var map = MMap[Expr, Expr]()
    
    def transform(tree: Expr, ctx: Expr => Expr): Unit = tree match {
      case nl: NilList => map += nl -> ctx(ex)
      case cons@Cons(h, t) =>
        map += cons -> ctx(ex)
        transform(h, se => u.Car(ctx(se)))
        transform(t, se => u.Cdr(ctx(se)))
      // variable supported as atoms
      case v: Variable =>
        map += v -> ctx(ex)
      case _: Car | _: Cdr =>
        throw new RuntimeException("Subtree should not be in example")
      case _ => throw new RuntimeException("Not supported")
    }
    
    transform(ex, identity)
    
    map.toMap
  }

  def allSubexpressions(tree: Expr): Set[Expr] = {
        
    collect({
      case nl: NilList => nl
      case c: Cons => c
      case car: Car => car
      case cdr: Cdr => cdr
      // variable supported as atoms
      case v: Variable => v
      case _ => throw new RuntimeException("Not supported")
    })(tree).toSet
    
  }
  
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
    
//    for ((key, expr) <- map)
//      expr.setType(key.getType)
    val modifiedMap = map.filterNot( _._1.isInstanceOf[NilList]  ).mapValues(Some(_))
    
    searchAndReplace({ e => modifiedMap.getOrElse(e, None) }, false)(tree)
  }
    
  def structureDifference(expr1: Expr, expr2: Expr) = {
    def rec(e1: Expr, e2: Expr, ctx: Expr => Expr): List[Expr => Expr] = (e1, e2) match {
      case (Atom(a1), Atom(a2)) => Nil
      case (Atom(a2), _) => List(ctx)
      case (Cons(h1, t1), Cons(h2, t2)) =>
        rec(h1, h2, x => ctx(u.Car(x))) ++
        rec(t1, t2, x => ctx(u.Cdr(x)))
    }
    
    rec(expr1, expr2, identity)
  }

}