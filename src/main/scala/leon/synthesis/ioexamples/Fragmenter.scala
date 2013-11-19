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
  
  def compare(expr1: Expr, expr2: Expr)(implicit map: Map[(Expr, Expr), Int]): (Int, Map[(Expr, Expr), Int]) = {
    var mutableMap = map
    
    def rec(expr1: Expr, expr2: Expr): Int = {
      val res = 
	      (expr1, expr2) match {
			    case (Cons(h1, t1), Cons(h2, t2)) =>	      
			      val headRes = rec(h1, h2)
			      val tailRes = rec(t1, t2)
			      // if one less, other greater return sign that these two are not comparable
			      if (headRes * tailRes < 0) throw new Exception
			      math.signum(headRes + tailRes)
			    case (Atom(_), _: Cons) => -1
			    case (_: Cons, Atom(_)) => 1
			    case (_, _) => 0			      
	      }
	      	      
      mutableMap += (expr1, expr2) -> res
      mutableMap += (expr2, expr1) -> -res
      res
    }
    
    val res =
	    try {
	    	rec(expr1, expr2)
	    } catch {
	      case _: Exception =>
		      mutableMap += (expr1, expr2) -> 0 
		      mutableMap += (expr2, expr1) -> 0
		      0
	    }
    
    (res, mutableMap.toMap)
  }
  
  def sort(inputExamples: List[Expr]) = {
    implicit var map = Map[(Expr, Expr), Int]()
    
    def compFun(a: Expr, b: Expr) = {
      val res = compare(a, b)
      map ++= res._2
      if (res._1 == 0) throw new Exception("Input examples should form a total order")      	
        
      res._1 < 0
    }
    
    inputExamples.sortWith((a, b) => compFun(a, b))
  }

}