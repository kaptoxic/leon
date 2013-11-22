package leon
package synthesis.ioexamples

import scala.collection.mutable.{ Map => MMap, TreeSet }

import purescala.Trees._
import purescala._

import insynth.util.logging.HasLogger

object Differencer extends HasLogger {
  
  val u = Util
  import Extractors._
  
  def differenceConstraints(expr1: Expr, expr2: Expr, boundVariable: Variable) = {
    def rec(e1: Expr, e2: Expr): List[Expr] = (e1, e2) match {
      case (`boundVariable`, _) => List(e2)
      case (_: NilList, _: NilList) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) =>
        rec(h1, h2) ++ rec(t1, t2)
      case (Car(a), Car(b)) => rec(a, b)
      case (Cdr(a), Cdr(b)) => rec(a, b)
      case (_, _) =>
        info("Throwing exception at matching " + e1 + " and " + e2)
        throw new Exception("Cannot satisfy constraint")
    }
    
    try { rec(expr1, expr2) } 
    catch {
      case _: Throwable =>
        info("Cannot satisfy constraint - thrown, for " + expr1 + " and " + expr2)
        Nil
    }    
  }
  
  def differences(expr1: Expr, expr2: Expr, boundVariable: Variable) = {
    // TODO expr2 itself is not needed
    val subexps = u.allSubexpressionsWithSubst(expr2) filter ( _._1 != boundVariable )
    
//    // all constraints are of the form (x -> Expr)
//    type Constraint = Expr
//    // substitution only substitutes x for E
//    type Substitution = Expr
//    
//	  def unify(c: List[Constraint]): Option[Substitution] =
//	    if (c.isEmpty) None
//	    else c.head match {
//	      case () if (a == b) =>
//	        unify(c.tail)
//	      case (x:TypeVar, t:Type) if ( ! occursIn(x.name, t) ) =>
//	        unify(substInConstraints(c.tail, x, t)) + (x, t)
//	      case (t:Type, x:TypeVar) if ( ! occursIn(x.name, t) ) =>
//	        unify(substInConstraints(c.tail, x, t)) + (x, t)
//	      case (TypeBool, TypeBool) | (TypeNat, TypeNat) =>
//	        unify(c.tail)
//	      case (TypeFun(s1, s2), TypeFun(t1, t2)) =>
//	        unify(c.tail :+ (s1, t1) :+ (s2, t2))
//	      case (t1, t2) =>
//	        throw TypeError("Could not unify: " + t1 + " with " + t2)
//	    }
    
    val substs =
	    for (subexppair <- subexps; subexp = subexppair._1;
	    constraints = differenceConstraints(expr1, subexp, boundVariable);
	  		if (constraints.distinct.size == 1) )
	      	yield {
	      		fine("for " + (expr1, subexp) + " yielding " + constraints.head)
	      		(constraints.head, subexppair._2)
	    		}
    
    if (substs.isEmpty)
      // if no substitutions exist, see if e2 is a subtree of e1
    	u.allSubexpressionsWithSubst(expr1).filter(_._1 == expr2)
    else
    	substs
  }

}