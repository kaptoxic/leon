package leon
package synthesis.ioexamples

import scala.collection.mutable.{ Map => MMap, TreeSet }

import purescala._
import Expressions._
import Definitions._
import Common._

import utils.logging.HasLogger

class Synthesizer extends HasLogger {

  type IO = (List[Expr], Expr)
  import Util._
  
  implicit var sortMap = MMap[IO, Int]()
  
  def synthesize(examples: List[IO]): Option[(Expr, TypedFunDef)] = {
//    val allIds = (Set[Variable]() /: examples) {
//      case (res, (in, out)) =>
//        res ++ TreeOps.collect({ case x: Variable => x })(in) ++
//          TreeOps.collect({ case x: Variable => x })(out)
//    }
//    
//    // do not know how to handle >1 variables so far
//    if (allIds.size != 1)
//      return None
      
    val inputVariable = Variable(FreshIdentifier("x", listType))
    // TEMPORARY
    assert(examples.forall(_._1.size == 1))
//    val sortedExamples = sort(examples, { x: IO => x._1 })    
    val sortedExamples = sort(examples, { x: IO => x._1.head })    
    
    val fragments = calculateFragments(sortedExamples, inputVariable :: Nil)
    // TEMPORARY
//    val predicates = calculatePredicates(sortedExamples map { _._1 }, inputVariable)
    assert(sortedExamples.forall(_._1.size == 1))
    val examplesByVariable = sortedExamples.map(_._1.head) :: Nil
    val predicates = calculatePredicates(examplesByVariable, inputVariable :: Nil)
    
    println(fragments.map({ case (a, b, c) => (a, b(w), c)}).mkString("\n"))
    println(predicates.mkString("\n"))
    
    (fragments, predicates) match {
      case (Some((fragments, a, b)), Some(p)) =>
        if (fragments.size == p.size) {
          // get type as type of output expression
          val resType = examples.head._2.getType
         
          // create a recursive function definition
          val newFun = new FunDef(
            FreshIdentifier("rec"),
            Nil,
            ValDef(inputVariable.id) :: Nil,
            resType
          ).typed

          assert(b.size == 1)
          val recursiveFragment =
            a(FunctionInvocation(newFun, Seq(b.head._2)))
          
          // last if-then case
          newFun.fd.body = Some(
            IfExpr(p.last, fragments.last, recursiveFragment)
          )
          
          // create final if-then-else expression by folding the "unfolded" fragments
          val ifExpr = 
            ((p.init zip fragments.init) :\ (FunctionInvocation( newFun, Seq(inputVariable) ): Expr)) {
              case ((pred, frag), elseExpr) =>
                IfExpr(pred, frag, elseExpr) 
            }          
          
          Some((ifExpr, newFun))
        } else
          throw new Exception("Do not know how to combine fragments in this case.")
      case _ =>
        None
    }
  }
  
  /*
   * returns:
   * - list of n fragments (last n one), for which some common form was identified
   * - path to this form ??
   * - mapping for substitution that would equate these fragments
   */
  def calculateFragments(examples: List[IO], xs: List[Variable]):
    Option[(List[Expr], Expr => Expr, Map[Variable, Expr])]= {
    entering("calculateFragments", examples, xs)
    
    val fragments = Fragmenter.constructFragments(examples, xs)
    info("fragments: " + fragments)
    
    val allDiffs =
	    (for((f1, f2) <- fragments zip fragments.tail) yield {
	      val diffs = Differencer.differences(f1, f2, xs)
	      info(s"diffs for $f1 and $f2 are $diffs")
		    diffs
	    })
    info("allDiffs: " + allDiffs.map(_.map({ case (k, v) => (k, v(w))})))
    
    // remove couple of last fragments if they have the same form
    var flag = true
    val (num, diffSet) = 
      // process in reverse order
	    ((1, allDiffs.last) /: allDiffs.init.reverse) {
	      case ((num, diffSet), elSet) =>
	        // TEMP, assume one variable
	        assert(diffSet.size == 1)
	        assert(diffSet.forall(_._1.size == 1))
	        val intersectionOfSubstitutions = diffSet.map(_._1.head._2).toSet intersect
  	        elSet.map(_._1.head._2).toSet
	        info("intersection of %s and %s is %s".format(diffSet, elSet, intersectionOfSubstitutions))
	        if (flag && !intersectionOfSubstitutions.isEmpty)
	          (num+1, diffSet.filter(intersectionOfSubstitutions contains _._1.head._2))
	        else {
	          flag = false
	          (num, diffSet)
	        }
	    }
    info("(num, diffSet): " + (num, diffSet))
    assert(diffSet.size == 1, "diffSet not size 1 " + diffSet)
    val (subst, restTree) = diffSet.head
    info(s"subst,restTree = ${(subst, restTree(w))}")
    
    // we check whether diffs entail same form for at least 2 consecutive examples
    // TODO check if subexpressions are equal (to be substituted in)
    if (num >= 2) {
      Some((fragments.dropRight(num), restTree, subst))
    }
    else None
  }
  
  /*
   * list of predicates, where each predicate is for list of variables
   * each list represents examples for a variable
   */
  def calculatePredicates(inputExamplesList: List[List[Expr]], xs: List[Variable]):
    Option[List[Expr]]= {
    entering("calculatePredicates", inputExamplesList, xs)
    
    val (predicatesList, predicatesFunsList) = (
      for ((inputExamples, x) <- inputExamplesList zip xs) yield {
        val atomExamples = inputExamples.map(substituteAllAtom)
        info("atomExamples: " + atomExamples)
      
        val predicatesFuns = Predicates.calculatePredicates(atomExamples, x)
        val predicates = predicatesFuns.map(_(x))
        info("predicates: " + predicates)
        (predicates, predicatesFuns)
      }
    ).unzip
    
    val (nums, diffSets, allDiffSets) = (
      for ((predicates, x) <- predicatesList zip xs) yield {
      
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
    	        // TEMP, assume one variable
    	        assert(diffSet.size == 1)
    	        if (flag && !(diffSet intersect elSet).isEmpty) (num+1, diffSet intersect elSet)
    	        else {
    	          flag = false
    	          (num, diffSet)
    	        }
    	    }
        info("(num, diffSet): " + (num, diffSet))
        assert(diffSet.size == 1)
        
        (num, diffSet, allDiffs)
      }
    ).unzip3
    info(s"(nums, diffSets, allDiffSets)=${(nums, diffSets, allDiffSets)}")

    assert(nums.forall( _ == nums.head))
    assert(diffSets.forall( _ == diffSets.head))
    assert(allDiffSets.forall( _ == allDiffSets.head))
    assert(predicatesFunsList.forall(_ == predicatesFunsList.head))
    assert(predicatesList.forall(_ == predicatesList.head))
    
    val predicatesFunList = predicatesFunsList.head
    val predicates = predicatesList.head
    val num = nums.head
    val allDiff = allDiffSets.head
    val diffSet = diffSets.head
    
    // we check whether diffs entail same form for at least 2 consecutive examples
    if (num >= 2) {
      Some(predicates.dropRight(num) :+
        predicatesFunList(allDiff.size - num)(diffSet.head.head._2))
    }
    else None
  }

}