package leon
package synthesis.ioexamples

import scala.collection.mutable.{ Map => MMap, TreeSet }

import purescala._
import Expressions._

import utils.logging.HasLogger

object Predicates extends HasLogger {
  
  val u = Util
  import Extractors._
    
  /*
   * requires expr1 < expr2 ??
   * returns a list of functions, that, when evaluated on both expr1 and expr2, give
   *  an atom and a composite, respectively
   * in the S-expression terminology: for expr1 returns an atom w and for expr2 (w.w)
   */
  def structureDifference(expr1: Expr, expr2: Expr) = {
    def rec(e1: Expr, e2: Expr, ctx: Expr => Expr): List[Expr => Expr] = (e1, e2) match {
      case (Atom(a1), Atom(a2)) => Nil
      case (Atom(a2), _) => List(ctx)
      case (Cons(h1, t1), Cons(h2, t2)) =>
        rec(h1, h2, x => ctx(u.Car(x))) ++
        rec(t1, t2, x => ctx(u.Cdr(x)))
      // TODO previous case is covered by Composite
      case (CaseClass(ct1, args1), CaseClass(ct2, args2)) if ct1 == ct2 =>
        (List[Expr => Expr]() /: (args1 zip args2 zip ct1.fields)) {
          case (acc, ((arg1, arg2), field)) =>
            acc ++ rec(arg1, arg2, x => ctx(CaseClassSelector(ct1, x, field.id)))
        }
      // case where a composite is in another composite (just do first argument now)
      // TODO generalize later
      case (cc1@CaseClass(ct1, args1), CaseClass(ct2, args2)) if args2.head == cc1 =>
        ((x: Expr) => ctx(CaseClassSelector(ct2, x, ct2.fields.head.id))) :: Nil
    }
    
    rec(expr1, expr2, identity)
  }
  
  /**
   * @param inputExamples sequence of trees <x_i, ..., x_j> such that x_i < x_i+1 for all i<i<j (well ordered
   * examples)
   * while intervals [x_i, x_i+1] are well ordered (well ordered intervals)
   * @param variable w that represents an atom
   * @return for an example interval x_i to x_{i+1} = y_n, for the sequence of trees <y_1 = x_i, ..., x_j = y_n>
   * returns a covering chain <y2, ..., y_n> (excluding first, including last tree)
   * similar to finding a covering chain [Summers, 1971]
   */
  def findChain(inputExamples: List[Expr], w: Expr = u.w) = {
    
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
      entering("traverse", x, x1)
      if (compFun(x, x1) == 0) {
        info("0 case")
        Nil
      }
      else (x, x1) match {
        case (Atom(_), _) =>
          info("Atom case")
          // note that we can do this since its total order and thus chain goes extremely left or right
          // remove the first one since it is an atom
        	u.allSubexpressions(x1).toList.sortWith{ case (a, b) => compFun(a, b) < 0 }.tail filterNot
          	{ _ == x }
        case (Cons(h1, t1), Cons(h2, t2)) =>
          info("Cons case")
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

        // TODO for now, this is fixed for size 2
        case (CaseClass(ct1, h1 :: t1 :: Nil), CaseClass(ct2, h2 :: t2 :: Nil)) if ct1 == ct2 =>
          info("Case class case")
          if (compFun(h1, h2) == 0) {
            val growLeft = CaseClass(ct1, t1 :: w :: Nil)
            val growRight = CaseClass(ct1, w :: t1 :: Nil)
//            assert(compFun(t2, growLeft) == 0 || compFun(t2, growRight) == 0,
//              "t2 " + t2 + " growLeft " + growLeft + " growRight " + growRight +
//            	"compFun(t2, growLeft) == 0" + (compFun(t2, growLeft) == 0) +
//              "compFun(t2, growRight) == 0" + (compFun(t2, growRight) == 0))
            assert(compFun(t2, growLeft) >= 0 || compFun(t2, growRight) >= 0)
            
            val successor = 
              if (compFun(t2, growLeft) == -2) growRight
              else growLeft

            CaseClass(ct1, h1 :: successor :: Nil) +:
              traverse(successor, t2).map(e => CaseClass(ct1, h1 :: e :: Nil))
          } else {
            // well ordered
            assert(compFun(t1, t2) == 0)
            
            val growLeft = CaseClass(ct1, h1 :: w :: Nil)
            val growRight = CaseClass(ct1, w :: h1 :: Nil)
//            assert(compFun(h2, growLeft) == 0 || compFun(h2, growRight) == 0)
            assert(compFun(h2, growLeft) >= 0 || compFun(h2, growRight) >= 0)
            
            val successor = 
              if (compFun(h2, growLeft) == -2) growRight
              else growLeft
              
            CaseClass(ct1, successor :: t1 :: Nil) +:
              traverse(successor, h2).map(e => CaseClass(ct1, e :: t1 :: Nil))
          }
//        case _ =>
//          throw new Exception("Input examples not in well order or not sorted")
      }
        
    }
    
    assert(inputExamples.zip(inputExamples.tail).size == inputExamples.size - 1)
    for ( (x, x1) <- inputExamples.zip(inputExamples.tail) )
    	yield {
      	info("for (x, x1) " + (x, x1) + " got " + traverse(x, x1))
	      traverse(x, x1)
	    }
  }
  
  /*
   * given a list of expressions (a chain in the lattice),
   *  returns a function that given an expression x returns a subexpression of x,
   *  at which the trees in the chain differ
   */
  def findPredicate(chain: List[Expr]) = {
    entering("findPredicate", chain)
    require(chain.size >= 2)

    val differencesLists =
      for( (x, x1) <- chain.zip(chain.tail) ) yield {
        structureDifference(x, x1)
      }
      // map { Atom(_) }
    
    assert( differencesLists.size > 0 )
    assert( differencesLists forall { _.size == 1})
    val differences = differencesLists.map(_.head)
    info("differences: " + differences.map(x => x(Util.w)))
    
    (x: Expr) => {
      val sequence = differences.map(_(x)).toSeq
      if (sequence.size >= 2) 
        Or(sequence)
      else
        sequence.head
    }
  }
  
  def calculatePredicates(inputExamples: List[List[Expr]], vars: List[Variable]):
    List[List[Expr=>Expr]] = {
    
    for((inputExample, v) <- (inputExamples zip vars)) yield
      calculatePredicates(inputExample, v)

  }

  /*
   * generate, for each chain x_i, ..., x_n that connects given examples, predicates p_i as paths
   *  to the subexpression for which p_i(x_i) is atom but p_i(x_(i+1)) is not
   */
  def calculatePredicates(inputExamples: List[Expr], v: Variable): List[Expr=>Expr] = {
    // note that this can find multiple chains
    val chain = findChain(inputExamples, v)
    val chainIncludingStart = chain.zip(inputExamples.init).collect {
      case (chain, start) if !chain.isEmpty => start :: chain
    }
    
    info("chainIncludingStart: " + chainIncludingStart)
    chainIncludingStart.map{ findPredicate(_) }
  }

}