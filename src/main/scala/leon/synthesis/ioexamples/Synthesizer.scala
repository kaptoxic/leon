package leon
package synthesis.ioexamples

import scala.collection.mutable.{ Map => MMap, TreeSet }

import purescala.Trees._
import purescala._
import purescala.Definitions._
import purescala.Common._

import insynth.util.logging.HasLogger

class Synthesizer extends HasLogger {

  type IO = (Expr, Expr)
  import Util._
  
  implicit var sortMap = MMap[IO, Int]()
  
  def synthesize(examples: List[IO]): Option[(Expr, FunDef)] = {
//    val allIds = (Set[Variable]() /: examples) {
//      case (res, (in, out)) =>
//        res ++ TreeOps.collect({ case x: Variable => x })(in) ++
//          TreeOps.collect({ case x: Variable => x })(out)
//    }
//    
//    // do not know how to handle >1 variables so far
//    if (allIds.size != 1)
//      return None
      
    val inputVariable = Variable(FreshIdentifier("x", true)).setType(listType)
    val sortedExamples = sort(examples, { x: IO => x._1 })    
    
    val fragments = calculateFragments(sortedExamples, inputVariable)
    val predicates = calculatePredicates(sortedExamples map { _._1 }, inputVariable)
    
    (fragments, predicates) match {
      case (Some((fragments, a, b)), Some(p)) =>
        if (fragments.size == p.size) {
          val resType = examples.head._2.getType
         
          val newFun = new FunDef(FreshIdentifier("rec", true), resType, Seq(VarDecl(inputVariable.id, inputVariable.getType)) )

          val recursiveFragment =
            a(FunctionInvocation(newFun, Seq(b)))
          
          newFun.body = Some(
            IfExpr(p.last, fragments.last, recursiveFragment)
          )
          
          val ifExpr = 
            ((p.init zip fragments.init) :\ (FunctionInvocation( newFun, Seq(inputVariable) ): Expr)) {
              case ((pred, frag), elseExpr) =>
                IfExpr(pred, frag, elseExpr) 
            }          
          
          Some(ifExpr, newFun)
        } else
          throw new Exception("Do not know how to combine fragments in this case.")
      case _ =>
        None
    }
  }
  
  def calculateFragments(examples: List[(Expr, Expr)], x: Variable):
    Option[(List[Expr], Expr => Expr, Expr)]= {
    entering("calculateFragments", examples, x)
    
    val fragments = Fragmenter.constructFragments(examples, x)
    info("fragments: " + fragments)
    
    val allDiffs =
	    (for((f1, f2) <- fragments zip fragments.tail) yield {
	      val diffs = Differencer.differences(f1, f2, x)
		    diffs
	    })
    info("allDiffs: " + allDiffs)
    
    var flag = true
    val (num, diffSet) = 
	    ((1, allDiffs.last) /: allDiffs.init.reverse) {
	      case ((num, diffSet), elSet) =>
	        val intersectionOfSubstitutions = diffSet.map(_._1) intersect elSet.map(_._1)
	        info("intersection of %s and %s is %s".format(diffSet, elSet, intersectionOfSubstitutions))
	        if (flag && !intersectionOfSubstitutions.isEmpty)
	          (num+1, diffSet.filter(intersectionOfSubstitutions contains _._1))
	        else {
	          flag = false
	          (num, diffSet)
	        }
	    }
    info("(num, diffSet): " + (num, diffSet))
    assert(diffSet.size == 1, "diffSet not size 1 " + diffSet)
    val (subst, restTree) = diffSet.head
    
    // we check whether diffs entail same form for at least 2 consecutive examples
    // TODO check if subexpressions are equal (to be substituted in)
    if (num >= 2) {
      Some(fragments.dropRight(num), restTree, subst)
    }
    else None
  }
  
  def calculatePredicates(inputExamples: List[Expr], x: Variable): Option[List[Expr]]= {
    entering("calculatePredicates", inputExamples, x)
    
    val atomExamples = inputExamples.map(substituteAllAtom)
    info("atomExamples: " + atomExamples)
    
    val predicatesFuns = Predicates.calculatePredicates(atomExamples)
    
    val predicates = predicatesFuns.map(_(x))
    info("predicates: " + predicates)
    
    val allDiffs =
	    for((f1, f2) <- predicates zip predicates.tail) yield {
	      val diffs = Differencer.differences(f1, f2, x).map(_._1)
		    diffs.toSet
	    }
    info("allDiffs: " + allDiffs)
    
    var flag = true
    val (num, diffSet) = 
	    ((1, allDiffs.last) /: allDiffs.init.reverse) {
	      case ((num, diffSet), elSet) =>
	        if (flag && !(diffSet intersect elSet).isEmpty) (num+1, diffSet intersect elSet)
	        else {
	          flag = false
	          (num, diffSet)
	        }
	    }
    info("(num, diffSet): " + (num, diffSet))
    assert(diffSet.size == 1)
    
    // we check whether diffs entail same form for at least 2 consecutive examples
    if (num >= 2) {
      Some(predicates.dropRight(num) :+ predicatesFuns(allDiffs.size - num)(diffSet.head))
    }
    else None
  }

}