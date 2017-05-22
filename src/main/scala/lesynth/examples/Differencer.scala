package leon
package synthesis.ioexamples

import scala.collection.mutable.{ Map => MMap }

import purescala.Trees._
import purescala._

import insynth.util.logging.HasLogger

object Differencer {
  
  import Extractors._
  
  def differences(expr1: Expr, expr2: Expr, refVar: Variable) = {
    def rec(e1: Expr, e2: Expr): Seq[(Expr, (Expr, Expr))] = (e1, e2) match {
      case (CaseClass(cd1, children1), CaseClass(cd2, children2)) if cd1 == cd2 =>
        val childDiffs =
          for (((c1, c2), f) <- children1 zip children2 zip cd1.fields; (pos, diff) <- rec(c1, c2))
            yield (CaseClassSelector(cd1, pos, f.id), diff)
        
        childDiffs
      case (CaseClass(cd1, children1), CaseClass(cd2, children2) ) =>
        Seq( (refVar, (CaseClassInstanceOf(cd1, refVar), CaseClassInstanceOf(cd2, refVar))) ) 
      case (_, _) =>
        throw new Exception("Cannot satisfy constraint")
    }
    
//    try {
      rec(expr1, expr2)
//    } 
//    catch {
//      case _: Throwable =>
//        Nil
//    }
  }
  
  def differences(exprs: List[Expr], refVar: Variable) = {
    // returns a map from list of differentiating terms to expressions in the partition
    def rec(exprs: List[Expr], acc: Expr): List[(List[Expr], Seq[Expr])] = {
      println("rec called with:\n" + exprs.mkString("\n"))
      
      // TODO hack -- we handle only ADTs at the moment
      if (!exprs.forall( _.isInstanceOf[CaseClass] ))
        return List( (Nil, exprs) )
      
      // partition by case class
      val partitions =
        exprs.groupBy {
          case CaseClass(classDef, exprs) => classDef
        } toList
      
      // if there are more than one partitions
      if (partitions.size > 1) {
        val (sizeOneParts, sizeMoreParts) = partitions.partition( _._2.size <= 1 )
        
        val recursiveDiffs = 
          // for partitions that have >1 expression
          for ( (diff, parts) <- sizeMoreParts;
            // recursively partition the partition parts
            (innerDiffs, innerParts) <- rec(parts, acc) ) yield
              // add this diff to the list of diffs
              (CaseClassInstanceOf(diff, acc) :: innerDiffs, innerParts)
            
        sizeOneParts.map({ case (cd, parts) => (List(CaseClassInstanceOf(cd, acc)), parts) }) :::
          recursiveDiffs
      } else {
        // classDef of all expressions in the partition
        val classDef = partitions.head._1

        // if no fields we do not have a distinguishing expression
        if (classDef.fields.isEmpty)
          List(( Nil, exprs ))
        else {
          // list of field expressions
          val subExprs =
            ((Seq.fill(classDef.fields.size)(Map[Expr, Expr]())) /: exprs.reverse) {
              case (res, expr@CaseClass(classDef, exprs)) =>          
                assert(res.size == exprs.size)
                (exprs zip res) map { case (x, xs) => xs + (expr -> x) }
            }
//          for (subExpr <- subExprs)
//            assert(subExpr.values.toSet == exprs.toSet, "Values:\n" + subExpr.values.toSet.mkString("\n") +
//              " not equal to set of all expressions\n" + exprs.toSet.mkString("\n"))
          
          // recursively invoke for all field expressions
          var left = subExprs zip classDef.fields
          // maintain final and left-to-do partitions
          var currentPartitions: List[(List[Expr], List[Expr])] = List((Nil, exprs))
          var finalPartitions = List[(List[Expr], List[Expr])]()
          
          while(!left.isEmpty && !currentPartitions.isEmpty) {
            val (exprToSubExpr, field) = left.head
            println("Currently processing: " + (exprToSubExpr.values, field))
            left = left.tail
            
            val subToExprs = (Map[Expr, Set[Expr]]() /: exprToSubExpr) {
              case (res, (e, subE)) =>
                res + (subE -> (res.getOrElse(subE, Set()) + e) )
            }
            
            val newPartitions =
              for((diffs, parts) <- currentPartitions;
                  // get subexpression for corresponding partition expressions
                  subParts = parts map exprToSubExpr;
                  _ = println("subparts: " + subParts);
                  (innerDiffs, innerParts) <-
                    rec(subParts, CaseClassSelector(classDef, acc, field.id))) yield {
                (innerDiffs ::: diffs, innerParts.distinct.map(subToExprs).flatten.toList)
              }
            println("newPartitions:\n" + newPartitions.mkString("\n"))
            val (singletons, biggerParts) = newPartitions.partition( p => p._2.size <= 1 )
            
            // TODO why does this return size 0 parititon
            finalPartitions = finalPartitions ::: singletons.filterNot(_._2.isEmpty)
            currentPartitions = biggerParts
            println("currentPartitions:\n" + currentPartitions.mkString("\n"))
          }
          assert(finalPartitions.size == exprs.distinct.size, "Out of loop, final partitions\n" +
            finalPartitions.size + ", expressions\n" + exprs.distinct.size)
          
          val res =
            finalPartitions
//            // recursively partition subexpressions
//            (List((Nil, exprs)) :/ (subExprs zip classDef.fields) {
//              case (parts, (subExpr, f)) =>
//                rec(subExpr, CaseClassSelector(classDef, acc, f.id))          
//            }

          // TODO this is a limitation (may succeed to partition only within one subexpression)
          // find the one that partitions into sizes of 1
          println("Found paritions: " + res.mkString("\n"))
          res
//          res.find( m => m.keys.forall { k => k.size == 1 } ).get
        }
      }
    }
    
    assert(exprs.forall(_.isInstanceOf[CaseClass]))

    rec(exprs, refVar)
  }
  
//  def differences(expr1: Expr, expr2: Expr, boundVariable: Variable) = {
//    // TODO expr2 itself is not needed
//    val subexps = allSubexpressionsWithSubst(expr2) filter ( _._1 != boundVariable )
//    
////    // all constraints are of the form (x -> Expr)
////    type Constraint = Expr
////    // substitution only substitutes x for E
////    type Substitution = Expr
////    
////	  def unify(c: List[Constraint]): Option[Substitution] =
////	    if (c.isEmpty) None
////	    else c.head match {
////	      case () if (a == b) =>
////	        unify(c.tail)
////	      case (x:TypeVar, t:Type) if ( ! occursIn(x.name, t) ) =>
////	        unify(substInConstraints(c.tail, x, t)) + (x, t)
////	      case (t:Type, x:TypeVar) if ( ! occursIn(x.name, t) ) =>
////	        unify(substInConstraints(c.tail, x, t)) + (x, t)
////	      case (TypeBool, TypeBool) | (TypeNat, TypeNat) =>
////	        unify(c.tail)
////	      case (TypeFun(s1, s2), TypeFun(t1, t2)) =>
////	        unify(c.tail :+ (s1, t1) :+ (s2, t2))
////	      case (t1, t2) =>
////	        throw TypeError("Could not unify: " + t1 + " with " + t2)
////	    }
//    
//    val substs =
//	    for (subexppair <- subexps; subexp = subexppair._1;
//	    constraints = differenceConstraints(expr1, subexp, boundVariable);
//	  		if (constraints.distinct.size == 1) )
//	      	yield {
//	      		(constraints.head, subexppair._2)
//	    		}
//    
//    if (substs.isEmpty)
//      // if no substitutions exist, see if e2 is a subtree of e1
//    	allSubexpressionsWithSubst(expr1).filter(_._1 == expr2)
//    else
//    	substs
//  }
  
//  def allSubexpressionsWithSubst(tree: Expr): List[(Expr, Expr => Expr)] = {
//    
//    def rec(e: Expr, ctx: Expr => Expr): List[(Expr, Expr => Expr)] = e match {
//      case Atom(a1) => List((e, ctx))
//      case Cons(h2, t2) =>
//        (e, ctx) :: rec(h2, x => ctx(Cons(x, t2))) ++ rec(t2, x => ctx(Cons(h2, x)))
//    }
//    
//    rec(tree, identity)
//  }

}