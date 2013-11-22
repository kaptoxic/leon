package leon
package test.ioexamples

import leon.synthesis.ioexamples._
import purescala.Trees._
import purescala._

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers._

class PredicatesTest extends FunSuite {
  
  import ExampleInputs._
  import Extractors._
  import Util._
  import TreeOps._
  
  import Predicates._

  test("predicate difference") {
    val x = ellABrlBlABrr
    val y = ellABrllABrlABrr
    
    val res = structureDifference(x, y)
    res.size should be (1)
    res(0)(nil) should be (Cdr(Car(nil)))
//    res(0)((ellABrlBlABrr)) should be (List(varB))
//    res(0)((ellABrllABrlABrr)) should be (List(ieLP_AB_RP))
  }

  test("find chain") {
    val inputExamples = List(elAr, elABCr).map( substituteAllAtom )
    val correctChain = List(List(elWWr, elWWWr).map (substituteAllAtom))
    
    // ensure all input examples have atoms replaced with w
    for (ex <- inputExamples) {
      val gathered = (List[Expr]() /: allSubexpressions(ex)) {
        (res, el) => res ++ collect({ case Atom(e) => e })(el)
      }.distinct
      gathered.size should be (1)
      gathered.head should be (w)
    }
    for (permutation <- inputExamples.permutations) {
      assert(permutation.size == inputExamples.size)
      
      sort(permutation) should be (inputExamples)
    }
    
    val res = findChain(inputExamples)
    res.size should be (1)
    res should be (correctChain)
  }

  test("find chain w -> (w (((w w) w) w))") {
    val inputExamples = List(w, elllWdWrdWrdWr)
    val correctChain = List(List(elWdWr, ellWdWrdWr, elllWdWrdWrdWr))
    
    val res = findChain(inputExamples)
    res.size should be (1)
    res should be (correctChain)
  }

  test("find chain (... w) -> (... (((w w) w) w))") {
    val inputExamples = List(w, elllWdWrdWrdWr)
    val correctChain = List(elWdWr, ellWdWrdWr, elllWdWrdWrdWr)
    
    for(leftExpr <- List(elWdWr, ellWdWrdWr, elllWdWrdWrdWr,
      elBlABrr, ellABrlBlABrr, ellABrllABrlABrr, elBlABrlABrr).map(substituteAllAtom)) {      
	    val res = findChain(inputExamples map { Cons(leftExpr, _) })
	    res.size should be (1)
	    res.head should be (correctChain map { Cons(leftExpr, _) })
    }
  }

  test("find chain (... w) -> (.. ((w w) w)) -> (... (((w w) w) w))") {
    val inputExamples = List(w, ellWdWrdWr, elllWdWrdWrdWr)
    val correctChain = List(
      List(elWdWr, ellWdWrdWr),
      List(elllWdWrdWrdWr)
    )
    
    for(leftExpr <- List(
      elWdWr, ellWdWrdWr, elllWdWrdWrdWr,
      elBlABrr, ellABrlBlABrr, ellABrllABrlABrr, elBlABrlABrr
      ).map(substituteAllAtom)) {      
	    val res = findChain(inputExamples map { Cons(leftExpr, _) })
	    res.size should be (2)
	    res should be (correctChain.map(_ map { Cons(leftExpr, _) }))
    }
  }

  test("find predicates") {
    val inputExamples = List(
      ieunpack1, ieunpack2, ieunpack3, ieunpack4
    ).map(substituteAllAtom)
    assert(sort(inputExamples) == inputExamples)
    
    val chain = findChain(inputExamples)
    chain.size should be (3)
    for ((inner, correct) <- chain zip inputExamples.tail) {
      inner should be (List(correct))
    } 
    
    val chainIncludingStart = chain.zip(inputExamples.init).map {
      case (chain, start) => start :: chain
    }
    
    val predicates = chainIncludingStart.map{ findPredicate(_) }
    predicates.size should be (3)
    for ((predicate, given) <- predicates zip List(
      predUnpack1, predUnpack2, predUnpack3
    ))
      predicate(w) should be (given)
  }
  
  test("find predicates for unpack") {
    val inputExamples = List(
      ieunpack1, ieunpack2, ieunpack3, ieunpack4
    ).map(substituteAllAtom)
    assert(sort(inputExamples) == inputExamples)
    
    val chain = findChain(inputExamples)
    chain.size should be (3)
    for ((inner, correct) <- chain zip inputExamples.tail) {
      inner should be (List(correct))
    } 
    
    val chainIncludingStart = chain.zip(inputExamples.init).map {
      case (chain, start) => start :: chain
    }
    
    val predicates = chainIncludingStart.map{ findPredicate(_) }
    predicates.size should be (3)
    for ((predicate, given) <- predicates zip List(
      predUnpack1, predUnpack2, predUnpack3
    ))
      predicate(w) should be (given)
  }

}