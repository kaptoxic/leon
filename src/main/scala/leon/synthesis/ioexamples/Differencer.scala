package leon
package synthesis.ioexamples

import scala.collection.mutable.{ Map => MMap, TreeSet }

import purescala._
import Expressions._

import utils.logging.HasLogger

object Differencer extends HasLogger {
  
  val u = Util
  import Extractors._
  
//  def differenceConstraints(expr1: Expr, expr2: Expr, boundVariables: Iterable[Variable]):
//    Map[Variable, List[Expr]] = {
//    val pairs =
//      for (boundVariable <- boundVariables) yield
//        (boundVariable, differenceConstraints(expr1, expr2, boundVariable))
//    
//    pairs.toMap
//  }
  
  def differenceConstraints(expr1: Expr, expr2: Expr, boundVariable: Variable):
    List[(Variable, Expr)] =
    differenceConstraints(expr1, expr2, boundVariable :: Nil)
  
  def differenceConstraints(expr1: Expr, expr2: Expr, boundVariables: List[Variable]):
    List[(Variable, Expr)] = {
    entering("differenceConstraints", expr1, expr2, boundVariables)
    
    def rec(e1: Expr, e2: Expr): List[(Variable, Expr)] =
    {
      entering("rec", e1, e2)
      (e1, e2) match {
//      case (`boundVariable`, _) => List(e2)
//      case (v1: Variable, v2: Variable) if v1 == v2 =>
//        Nil
      case (v: Variable, _) if boundVariables contains v =>
        (v -> e2) :: Nil
      case (_: NilList, _: NilList) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) =>
        rec(h1, h2) ++ rec(t1, t2)
      case (Car(a), Car(b)) => rec(a, b)
      case (Cdr(a), Cdr(b)) => rec(a, b)
      case (nl1@CaseClass(ct1, args1), nl2@CaseClass(ct2, args2)) if ct1 == ct2 =>
        (List[(Variable, Expr)]() /: (args1 zip args2)) {
          case (curr, (arg1, arg2)) =>
            curr ++ rec(arg1, arg2)
        }
        
      // add operator here and check equality of literal operands
      case (Operator(args1, _), Operator(args2, fn2)) if fn2(args1) == e1 =>
        (List[(Variable, Expr)]() /: (args1 zip args2)) {
          case (curr, (arg1, arg2)) =>
            curr ++ rec(arg1, arg2)
        }
        
      case (ccs1@CaseClassSelector(ct1, e1, f1), ccs2@CaseClassSelector(ct2, e2, f2)) if
        ct1 == ct2 && f1 == f2 =>
        rec(e1, e2)
//      case (nl1@CaseClass(ct1, args1), nl2@CaseClass(ct2, args2)) =>
//        println("WTF" + ct1 == ct2)
//        (List[Expr]() /: (args1 zip args2)) {
//          case (curr, (arg1, arg2)) =>
//            curr ++ rec(arg1, arg2)
//        }
          
      case (_, _) =>
        info("Throwing exception at matching " + e1 + " and " + e2)
        throw new Exception("Cannot satisfy constraint")
    }
    }
    
    try { rec(expr1, expr2) } 
    catch {
      case _: Throwable =>
        info("Cannot satisfy constraint - thrown, for " + expr1 + " and " + expr2)
        Nil
    }    
  }
  
  // expr1 <= expr2, wrt to the lattice
  def differences(expr1: Expr, expr2: Expr, boundVariable: Variable):
    Iterable[(Map[Variable, Expr], Expr => Expr)] =
    differences(expr1, expr2, boundVariable :: Nil)
  
  /*
   * differences between expr1 and expr2 in terms of:
   * - mapping of variables into expressions, such that when this substitution is done on expr2
   *  (on it's subexpression as defined consequently), it is equal to expr1
   * - functions such that when evaluated on the found subexpressions return expr2
   */
  def differences(expr1: Expr, expr2: Expr, boundVariables: List[Variable]):
    Iterable[(Map[Variable, Expr], Expr => Expr)] = {
    // TODO expr2 itself is not needed
    // ignore substitutions for bound variables
    val subexps = u.subexpressionsToContexts(expr2)// filterNot ( boundVariables contains _._1 )
    info(s"subexps for $expr2:\n${subexps.map({ case (k,v) => (k,v(boundVariables.head)) })
      .mkString("\n")}")
      
    val substs =
	    for (subexppair <- subexps; subexp = subexppair._1;
	      _ = info(s"subexpression worked on: $subexp");
  	    constraints = differenceConstraints(expr1, subexp, boundVariables);
  	    constraintsMap = (Map[Variable, Set[Expr]]() /: constraints) {
  	      case (curr, (v, e)) => curr + (v -> (curr.getOrElse(v, Set()) + e))
        };
        _ = assert(constraintsMap.forall(_._2.size > 0));
	  		if (constraints.size > 0 && constraintsMap.values.forall(_.size == 1)) )
	      	yield {
	      	  val constraintsAsMap =
	      	    constraintsMap.map({
	      	      case (k, v) => (k, v.head)
      	      })
	      		fine("for " + (expr1, subexp) + " yielding " + constraintsAsMap)
	      		(constraintsAsMap, subexppair._2)
	    		}
    
//    if (substs.isEmpty) {
      // if no substitutions exist, see if e2 is a subtree of e1
//    	val ex1subexpressions = u.mapOfSubexpressions(expr1).filter(_._1 == expr2)
//    	assert(ex1subexpressions.size == 1)
//    	(Map[Variable, Expr](), ex1subexpressions.head._2) :: Nil
//      ???
//    }
//    else
  	substs
  }
  
  def areCompatible(d1: (Map[Variable, Expr], Expr => Expr), d2: (Map[Variable, Expr], Expr => Expr)):
    Option[(Map[Variable, Expr], Expr => Expr)] = {
    import u._
    
    val (m1, s1) = d1
    val (m2, s2) = d2
    
    val mapsCompatible =
      (true /: m1.keys) {
        case (current, key) =>
          current && (!m2.contains(key) || m2(key) == m1(key))
      }
    
    // we can use 'w' or anything here
    val subexpressionsCompatible =
      s1(w) == s2(w)
      
    if (mapsCompatible && subexpressionsCompatible)
      Some(m1 ++ m2, s1)
    else
      None
    
  }
  
  def differenceConstraintsRelaxed(expr1: Expr, expr2: Expr): List[(Expr, Expr)] = {
    entering("differenceConstraints", expr1, expr2)
    
    def rec(e1: Expr, e2: Expr): List[(Expr, Expr)] = {
      entering("rec", e1, e2)

      (e1, e2) match {
        case (v1: Variable, v2: Variable) if v1 == v2 =>
          Nil
        case (v: Variable, _) =>
          (v -> e2) :: Nil
        case (_: NilList, _: NilList) =>
          Nil
        case (Cons(h1, t1), Cons(h2, t2)) =>
          rec(h1, h2) ++ rec(t1, t2)
        case (Car(a), Car(b)) => rec(a, b)
        case (Cdr(a), Cdr(b)) => rec(a, b)
        case (nl1@CaseClass(ct1, args1), nl2@CaseClass(ct2, args2)) if ct1 == ct2 =>
          (List[(Expr, Expr)]() /: (args1 zip args2)) {
            case (curr, (arg1, arg2)) =>
              curr ++ rec(arg1, arg2)
          }
        case (nl1@CaseClass(ct1, args1), nl2@CaseClass(ct2, args2)) =>
          (nl1 -> nl2) :: Nil
          
        // add operator here and check equality of literal operands
        case (Operator(args1, _), Operator(args2, fn2)) if fn2(args1) == e1 =>
          (List[(Expr, Expr)]() /: (args1 zip args2)) {
            case (curr, (arg1, arg2)) =>
              curr ++ rec(arg1, arg2)
          }
          
        case (ccs1@CaseClassSelector(ct1, e1, f1), ccs2@CaseClassSelector(ct2, e2, f2)) if
          ct1 == ct2 && f1 == f2 =>
          rec(e1, e2)
  //      case (nl1@CaseClass(ct1, args1), nl2@CaseClass(ct2, args2)) =>
  //        println("WTF" + ct1 == ct2)
  //        (List[Expr]() /: (args1 zip args2)) {
  //          case (curr, (arg1, arg2)) =>
  //            curr ++ rec(arg1, arg2)
  //        }
            
        case (_, _) =>
          info("Throwing exception at matching " + e1 + " and " + e2)
          throw new Exception("Cannot satisfy constraint")
      }
    }
    
    try { rec(expr1, expr2) } 
    catch {
      case _: Throwable =>
        info("Cannot satisfy constraint - thrown, for " + expr1 + " and " + expr2)
        Nil
    }    
  }
  
  
//  def areCompatible(differences: (Map[Variable, Expr], Expr => Expr)):
//    Option[(Map[Variable, Expr], Expr => Expr)] = {
//    
//    differences.
//    
//    (None /: differences) {
//      case (current, difference) =>
//      case (current, difference) =>
//    }
//    
//  }
  
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

}