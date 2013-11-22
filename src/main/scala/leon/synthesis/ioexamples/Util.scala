package leon
package synthesis.ioexamples

import scala.collection.mutable.{ Map => MMap, TreeSet }

import purescala.{ Trees => ot }
import purescala.Trees._
import purescala.TypeTrees._
import purescala.TreeOps
import purescala.Common.FreshIdentifier
import purescala.Extractors

object Util {
  
  import Extractors._
  import TreeOps._
  
  // avoid typing problems
  val listType = ListType(BooleanType)
  
  val w = Variable(FreshIdentifier("w")).setType(listType)
  val nil = ot.NilList(listType)
  
  def Cons(h: Expr, t: Expr) = ot.Cons(h, t).setType(listType)
  def Car(l: Expr) = ot.Car(l).setType(listType)
  def Cdr(l: Expr) = ot.Cdr(l).setType(listType)
  
  def isAtom(e: Expr) = e match {
	  case Atom(_) => true
	  case _ => false
	}

  def substituteAtom(e: Expr) = e match {
	  case Atom(_) => Some(w)
	  case _ => None
	}
  
  def substituteAllAtom(e: Expr) = searchAndReplace( substituteAtom )(e)

//  def substituteAtom(e: Expr) = e match {
//	  case Atom(_) => w
//	  case _ => e
//	}
  
  def mapOfSubexpressions(ex: Expr): Map[Expr, (Expr => Expr)] = {
    var map = MMap[Expr, (Expr => Expr)]()
    
    def transform(tree: Expr, ctx: Expr => Expr): Unit = tree match {
      case nl: NilList => map += nl -> ctx
      case cons@Cons(h, t) =>
        map += cons -> ctx
        transform(h, se => Car(ctx(se)))
        transform(t, se => Cdr(ctx(se)))
      // variable supported as atoms
      case v: Variable =>
        map += v -> ctx
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

  def allSubexpressionsWithSubst(tree: Expr): List[(Expr, Expr => Expr)] = {
    
    def rec(e: Expr, ctx: Expr => Expr): List[(Expr, Expr => Expr)] = e match {
      case Atom(a1) => List((e, ctx))
      case Cons(h2, t2) =>
        (e, ctx) :: rec(h2, x => ctx(Cons(x, t2))) ++ rec(t2, x => ctx(Cons(h2, x)))
    }
    
    rec(tree, identity)
  }

  def compare(expr1: Expr, expr2: Expr)(implicit map: Map[(Expr, Expr), Int]): (Int, Map[(Expr, Expr), Int]) = {
    var mutableMap = map
    
    def rec(expr1: Expr, expr2: Expr): Int = {
      if (mutableMap contains (expr1, expr2))
        return mutableMap((expr1, expr2))        
      
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
		      mutableMap += (expr1, expr2) -> -2 
		      mutableMap += (expr2, expr1) -> -2
		      -2
	    }
    
    (res, mutableMap.toMap)
  }
  
  def sort[T](inputExamples: List[T], convert: T=>Expr) = {
    implicit var map = Map[(Expr, Expr), Int]()
    
    def compFun(a: Expr, b: Expr) = {
      val res = compare(a, b)
      map ++= res._2
      if (res._1 == 0 || res._1 == -2) throw new Exception("Input examples should form a total order")        
       
      res._1 < 0
    }
    
    inputExamples.sortWith((a, b) => compFun(convert(a), convert(b)))
  }
  
  def sort(inputExamples: List[Expr]) = {
    implicit var map = Map[(Expr, Expr), Int]()
    
    def compFun(a: Expr, b: Expr) = {
      val res = compare(a, b)
      map ++= res._2
      if (res._1 == 0 || res._1 == -2) throw new Exception("Input examples should form a total order")      	
       
      res._1 < 0
    }
    
    inputExamples.sortWith((a, b) => compFun(a, b))
  }

}