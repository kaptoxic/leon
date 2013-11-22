package leon
package synthesis.ioexamples

import scala.collection.mutable.{ Map => MMap, TreeSet }

import purescala.Trees._
import purescala._

import insynth.util.logging.HasLogger

object Predicates extends HasLogger {
  
  val u = Util
  import TreeOps._
  import Extractors._
    
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
  
  /**
   * finds a covering chain [Summers, 1971]
   * @param inputExamples sequence of trees <x_i, ..., x_j> such that x_i < x_i+1 for all i<i<j (well ordered examples)
   * while intervals [x_i, x_i+1] are well ordered (well ordered intervals)
   * @return for a sequence of trees <y_1 = x_i, ..., x_j = y_n> returns a covering chain <y2, ..., y_n> (excluding first,
   * including last tree)
   */
  def findChain(inputExamples: List[Expr]) = {
  	import u.w
    
    implicit var compareMap = Map[(Expr, Expr), Int]()
    
    def compFun(a: Expr, b: Expr) = {
      val res = u.compare(a, b)
      compareMap ++= res._2
      res._1
    }
  	
//    var toDiscover = inputExamples
//	    new TreeSet[Expr]()(new Ordering[Expr]{
//				def compare(a: Expr, b: Expr) = compFun(a, b)
//	    })
//    toDiscover ++= inputExamples
    
    def traverse(x: Expr, x1: Expr): List[Expr] = {  	  
      if (compFun(x, x1) == 0) Nil
      else (x, x1) match {
        case (Atom(_), _) =>
          // note that we can do this since its total order and thus chain goes extremely left or right
          // remove the first one since it is an atom
        	u.allSubexpressions(x1).toList.sortWith{ case (a, b) => compFun(a, b) < 0 }.tail
        case (Cons(h1, t1), Cons(h2, t2)) =>
          if (compFun(h1, h2) == 0) {
            val growLeft = Cons(t1, w)
            val growRight = Cons(w, t1)
//            assert(compFun(t2, growLeft) == 0 || compFun(t2, growRight) == 0,
//              "t2 " + t2 + " growLeft " + growLeft + " growRight " + growRight +
//            	"compFun(t2, growLeft) == 0" + (compFun(t2, growLeft) == 0) +
//              "compFun(t2, growRight) == 0" + (compFun(t2, growRight) == 0))
            assert(compFun(t2, growLeft) >= 0 || compFun(t2, growRight) >= 0)
            
            val successor = 
              if (compFun(t2, growLeft) == -2) growRight
              else growLeft

            Cons(h1, successor) +: traverse(successor, t2).map(e => Cons(h1, e))
          } else {
            // well ordered
            assert(compFun(t1, t2) == 0)
            
            val growLeft = Cons(h1, w)
            val growRight = Cons(w, h1)
//            assert(compFun(h2, growLeft) == 0 || compFun(h2, growRight) == 0)
            assert(compFun(h2, growLeft) >= 0 || compFun(h2, growRight) >= 0)
            
            val successor = 
              if (compFun(h2, growLeft) == -2) growRight
              else growLeft
              
            Cons(successor, t1) +: traverse(successor, h2).map(e => Cons(e, t1))
          }
        case _ =>
          throw new Exception("Input examples not in well order or not sorted")
      }
        
    }
    
    assert(inputExamples.zip(inputExamples.tail).size == inputExamples.size - 1)
    for ( (x, x1) <- inputExamples.zip(inputExamples.tail) )
    	yield {
      	info("for (x, x1) " + (x, x1) + " got " + traverse(x, x1))
	      traverse(x, x1)
	    }
  }
  
  def findPredicate(chain: List[Expr]) = {
    require(chain.size >= 2)

    val differences =
      for( (x, x1) <- chain.zip(chain.tail) ) yield {
        structureDifference(x, x1)
      }
      // map { Atom(_) }
    
    assert( differences forall { _.size == 1})
    
    (x: Expr) => Or(differences.map(_.head(x)).toSeq)
  }
  
  def calculatePredicates(inputExamples: List[Expr]) = {
    val chain = findChain(inputExamples)
    val chainIncludingStart = chain.zip(inputExamples.init).map {
      case (chain, start) => start :: chain
    }
    
    chainIncludingStart.map{ findPredicate(_) }
  }

}