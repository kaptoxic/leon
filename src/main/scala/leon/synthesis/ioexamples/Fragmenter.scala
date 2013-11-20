package leon
package synthesis.ioexamples

import scala.collection.mutable.{ Map => MMap, TreeSet }

import purescala.Trees._
import purescala._

import insynth.util.logging.HasLogger

object Fragmenter extends HasLogger {
  
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
      val res = compare(a, b)
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
        	allSubexpressions(x1).toList.sortWith{ case (a, b) => compFun(a, b) < 0 }.tail
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
  
  def differences(expr1: Expr, expr2: Expr, boundVariable: Variable) = {
    // TODO expr2 itself is not needed
    val subexps = allSubexpressions(expr2) filter ( _ != boundVariable )
    
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
	    for (subexp <- subexps; constraints = differenceConstraints(expr1, subexp, boundVariable);
	  		if (constraints.distinct.size == 1) )
	      	yield {
	      		fine("for " + (expr1, subexp) + " yielding " + constraints.head)
	      		constraints.head
	    		}
    
    if (substs.isEmpty)
    	allSubexpressions(expr1) intersect Set(expr2)
    else
    	substs
  }

}