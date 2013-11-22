package leon
package test.ioexamples

import leon.synthesis.ioexamples._
import purescala.Trees._
import purescala._

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers._

class DifferencerTest extends FunSuite {
  
  import ExampleInputs._
  import Extractors._
  import Util._
  import TreeOps._

  import Differencer._
  
  test("differences constraints") {
    {
	    val diffs = differenceConstraints(elWr, Cons(Cdr(w), nil), w)
	    diffs.size should be (1)
	    diffs should be (List(Cdr(w)))
    }
    
    {
	    val diffs = differenceConstraints(fragUnpack3, Cons(Cons(Car(Cdr(x)), nil), Cons(Cdr(Cdr(x)), nil)), x).distinct
	    diffs.size should be (1)
	    diffs should be (List(Cdr(x)))      
    }
    
  }

  test("difference produced") {
    val fragments = List(fragUnpack2, fragUnpack3, fragUnpack4)
    
    val allDiffs =
	    for((f1, f2) <- fragments zip fragments.tail) yield {
	      val diffs = differences(f1, f2, x).map(_._1)
		    assert(diffs contains Cdr(x))
		    diffs.toSet
	    }
    
    allDiffs(0) intersect allDiffs(1) should be (Set(Cdr(x)))
    allDiffs(0) union allDiffs(1) should be (Set(Cdr(x), Car(x)))
  }
  
  test("differences produced for predicates") {
    val fragments = List(predUnpack1, predUnpack2, predUnpack3)
    
    val allDiffs =
	    for((f1, f2) <- fragments zip fragments.tail) yield {
	      val diffs = differences(f1, f2, w).map(_._1)
		    assert(diffs contains Cdr(w))
		    diffs.toSet
	    }
    
    allDiffs(0) intersect allDiffs(1) should be (Set(Cdr(w)))
    allDiffs(0) union allDiffs(1) should be (Set(Cdr(w)))
  }

  test("differences for fragments, unpack") {
    val fragments = List(fragUnpack2, fragUnpack3, fragUnpack4)
    
    val allDiffs =
	    for((f1, f2) <- fragments zip fragments.tail) yield {
	      val diffs = differences(f1, f2, x).map(_._1)
		    assert(diffs contains Cdr(x))
		    diffs.toSet
	    }
    
    allDiffs(0) intersect allDiffs(1) should be (Set(Cdr(x)))
    allDiffs(0) union allDiffs(1) should be (Set(Cdr(x), Car(x)))
  }

  
//    val diffs = differenceConstraints(nil, Cons(Cdr(varX), nil), w)
//    diffs.size should be (1)
//    diffs shuold be (List())

}