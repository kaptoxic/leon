package leon
package synthesis.ioexamples

import scala.collection.mutable.{ Map => MMap, TreeSet }

import purescala._
import purescala.{ Expressions => ot }
import Expressions._
import Types._
import ExprOps._
import Common.FreshIdentifier
import Extractors._

import leon.utils.logging._

object Util extends HasLogger {
  
  import Extractors._
  
  // avoid typing problems
  val listType = ListType(BooleanType)
  
  val w = Variable(FreshIdentifier("w", listType))
  val nil = NilList(listType)
  
  def Cons(h: Expr, t: Expr) = ot.Cons(h, t)
  def Car(l: Expr) = ot.Car(l)
  def Cdr(l: Expr) = ot.Cdr(l)
  
  def isAtom(e: Expr) = e match {
	  case Atom(_) => true
	  case _ => false
	}

  def substituteAtom(w: Expr)(e: Expr) = e match {
	  case Atom(_) => Some(w)
	  case _ => None
	}
  
  def substituteAllAtom(e: Expr) = postMap( substituteAtom(w) )(e)
  def substituteAllAtomWith(e: Expr)(w: Expr) = postMap( substituteAtom(w) )(e)

//  def substituteAtom(e: Expr) = e match {
//	  case Atom(_) => w
//	  case _ => e
//	}
  
  /**
   * return subexpressions x of ex as well as expressions that when evaluated on ex,
   *  return x 
   * @param ex
   * @return
   */
  def mapOfSubexpressionsToPathFunctions(ex: Expr): Map[Expr, (Expr => Expr)] = {
    var map = MMap[Expr, (Expr => Expr)]()
    
    def transform(tree: Expr, ctx: Expr => Expr): Unit = tree match {
      // Scala Case classes
      case nl@CaseClass(nilClass, args) if args.size == 0 =>
        map += nl -> ctx
          
      case cons@CaseClass(consClass, args) =>
        map += cons -> ctx
        assert(args.size == consClass.fields.size)
        for ((arg, f) <- args zip consClass.fields) {
          transform(arg, se => CaseClassSelector(consClass, ctx(se), f.id))
        }
        
//      case _: CaseClassSelector =>
//        throw new RuntimeException(s"Subtree $tree should not be in example")
      
      // S-expressions and hardcoded lists
      case nl: NilList => map += nl -> ctx
      case cons@Cons(h, t) =>
        map += cons -> ctx
        transform(h, se => Car(ctx(se)))
        transform(t, se => Cdr(ctx(se)))
      // variable supported as atoms
      case v: Variable =>
        map += v -> ctx
//      case _: Car | _: Cdr =>
//        throw new RuntimeException(s"Subtree $tree should not be in example")
      case e =>
        map += e -> ctx
//      case _ =>
//        throw new RuntimeException("Not supported")
    }
    
    transform(ex, identity)
    
    map.toMap
  }
  
  
  def mapOfSubexpressionsToPathFunctions(exs: Iterable[Expr]): Iterable[Map[Expr, (Expr => Expr)]] =
//    (Map.empty[Expr, (Expr => Expr)] /: (for (ex <- exs) yield mapOfSubexpressions(ex))) {
//      case (current, map) => map ++ current 
//    }
    for (ex <- exs) yield mapOfSubexpressionsToPathFunctions(ex)

  def allSubexpressions(tree: Expr): Set[Expr] = {
    
    collect[Expr]({
      case nl: NilList => Set(nl)
      case c: Cons => Set(c)
      case car: Car => Set(car)
      case cdr: Cdr => Set(cdr)
      // variable supported as atoms
      case c@Composite(exprs) => exprs.toSet + c
      case Atom(v) => Set(v)
      case _ => throw new RuntimeException("Not supported")
    })(tree)
    
  }

  /**
   * return subexpressions x of ex as well as functions that when evaluated on x,
   *  return ex
   * @param ex
   * @return
   */
  def subexpressionsToContexts(tree: Expr): List[(Expr, Expr => Expr)] = {
    
    def rec(e: Expr, ctx: Expr => Expr): List[(Expr, Expr => Expr)] = e match {
      case Atom(a1) => List((e, ctx))
      case Cons(h2, t2) =>
        (e, ctx) :: rec(h2, x => ctx(Cons(x, t2))) ++ rec(t2, x => ctx(Cons(h2, x)))
      case CaseClass(ct, args) =>
        (List((e, ctx)) /: (0 until args.size)) {
          case (curr, ind) =>
            val positionElement = args(ind)
            // argsList used just as optimization
            val argsList = args.toList
            val prefix = argsList.take(ind)
            val postfix = argsList.drop(ind).tail
            curr ::: rec(positionElement, x => ctx(CaseClass(ct, prefix ::: (x :: postfix))))
        }

    }
    
    rec(tree, identity)
  }

  def compare(expr1: Expr, expr2: Expr)(implicit map: Map[(Expr, Expr), Int]): (Int, Map[(Expr, Expr), Int]) = {
    entering("compare", expr1, expr2)
    var mutableMap = map
    
    def rec(expr1: Expr, expr2: Expr): Int = {
      entering("rec", expr1, expr2)
      if (mutableMap contains (expr1, expr2))
        return mutableMap((expr1, expr2))        
      
      val res = 
	      (expr1, expr2) match {
          case (Composite(args1), Composite(args2)) =>
            (0 /: (args1 zip args2)) {
              case (curr, (arg1, arg2)) =>
                val newRes = rec(arg1, arg2)
//			      // if one less, other greater return sign that these two are not comparable
                if (curr * newRes < 0) throw new Exception
                math.signum(newRes + curr)
//              case (0, (arg1, arg2)) =>
//                rec(arg1, arg2)
//              case (curr, _) =>
//                assert(curr == -1 || curr == 1)
//                curr
            }
			    case (Atom(_), Composite(_)) => -1
			    case (Composite(_), Atom(_)) => 1
			    case (_, _) =>
			      info(s"res = 0")
			      0			      
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
    
    exiting("compare", (res, mutableMap.toMap).toString)
    (res, mutableMap.toMap)
  }
  
  // sort in case multiple inputs
  def sort[T](inputExamples: List[T], convert: T=>Seq[Expr]) = {
    ???
  }
  
  def sort[T](inputExamples: List[T], convert: T=>Expr) = {
    // this is just to speed things up?
    implicit var map = Map[(Expr, Expr), Int]()
    
    def compFun(a: Expr, b: Expr) = {
      val res = compare(a, b)
      map ++= res._2
      if (res._1 == 0 || res._1 == -2) throw new Exception("Input examples should form a total order")        
       
      res._1 < 0
    }
    
    inputExamples.sortWith((a, b) => compFun(convert(a), convert(b)))
  }

  def sortWrtInputs(inputExamplesFragments: List[(List[Expr], Expr)]) = {
    implicit var map = Map[(Expr, Expr), Int]()
    
    def compFun(as: List[Expr], bs: List[Expr]) = {
      entering("compFun", as, bs)
      val res =
        ((0, map) /: (as zip bs)) {
          case ((res, map), (a, b)) if res == 0 || res == -2 => 
            compare(a, b)(map)
          case (curr, _) =>
            curr
        }
      map ++= res._2
      if (res._1 == 0 || res._1 == -2)
        throw new Exception(s"Input examples should form a total order (res._1=${res._1})")      	
       
      res._1 < 0
    }
    
    inputExamplesFragments.sortWith((a, b) => compFun(a._1, b._1))
  }
  
  def sort(inputExamples: List[Expr]) = {
    implicit var map = Map[(Expr, Expr), Int]()
    
    def compFun(a: Expr, b: Expr) = {
      val res = compare(a, b)
      map ++= res._2
      if (res._1 == 0 || res._1 == -2)
        throw new Exception(s"Input examples should form a total order (res._1=${res._1})")      	
       
      res._1 < 0
    }
    
    inputExamples.sortWith((a, b) => compFun(a, b))
  }
  
  implicit def diffToString(l: (Map[Expressions.Variable, Expressions.Expr],
    Expressions.Expr => Expressions.Expr) ) = s"${l._1}\n- ${l._2(w)}"
  implicit def diffsToString(l: Iterable[(Map[Expressions.Variable, Expressions.Expr],
    Expressions.Expr => Expressions.Expr)]) = l.map(diffToString).mkString("\n")
//  def println(s: String) = scala.Predef.println(s)
    
    
  /**
   * return subexpressions x of ex as well as expressions that when evaluated on ex,
   *  return x 
   * @param ex
   * @return
   */
  def subexpressionToPathFunctionsPairs(ex: Expr): Iterable[(Expr, (Expr => Expr))] = {
    var map = new collection.mutable.ListBuffer[(Expr, (Expr => Expr))]()
    
    def transform(tree: Expr, ctx: Expr => Expr): Unit = tree match {
      // Scala Case classes
      case nl@CaseClass(nilClass, args) if args.size == 0 =>
        map += nl -> ctx
          
      case cons@CaseClass(consClass, args) =>
        map += cons -> ctx
        assert(args.size == consClass.fields.size)
        for ((arg, f) <- args zip consClass.fields) {
          transform(arg, se => CaseClassSelector(consClass, ctx(se), f.id))
        }
        
//      case _: CaseClassSelector =>
//        throw new RuntimeException(s"Subtree $tree should not be in example")
      
      // S-expressions and hardcoded lists
      case nl: NilList => map += nl -> ctx
      case cons@Cons(h, t) =>
        map += cons -> ctx
        transform(h, se => Car(ctx(se)))
        transform(t, se => Cdr(ctx(se)))
      // variable supported as atoms
      case v: Variable =>
        map += v -> ctx
//      case _: Car | _: Cdr =>
//        throw new RuntimeException(s"Subtree $tree should not be in example")
      case e =>
        map += e -> ctx
//      case _ =>
//        throw new RuntimeException("Not supported")
    }
    
    transform(ex, identity)
    
    map.toList
  }

}