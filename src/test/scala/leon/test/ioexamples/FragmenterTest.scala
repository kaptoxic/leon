package leon
package test.ioexamples

import leon.synthesis.ioexamples._
import purescala.Trees._
import purescala.TreeOps

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers._

// TODO codegen evaluator params should be used but not yet in master
class EvaluationTest extends FunSuite {
  
  import ExampleInputs._
  import Fragmenter._

  import Util._

  def assertExpressionMapping(e: Expr)(implicit map: Map[Expr, Expr]) = {
    assert(map contains e)
    map(e) should be (e)
  }
  
  test("(A B) subexpressions") {
    
    val map = allSubexpressions(elABr)
        
    map.size should be (5)
    
    for(subexp <- List(varA, varB, Cons(varB, nil), nil, elABr))
      map contains subexp
  }
  
  test("(A B) map to subexpressions") {
    
    val map = mapOfSubexpressions(elABr)
        
    map.size should be (5)
    
    val x = elABr
    for((subtree, ops) <- List(
      (varA, Car(x)), (varB, Car(Cdr(x))), (Cons(varB, nil), Cdr(x)),
      (nil, Cdr(Cdr(x))), (elABr, x)
    ))
      map(subtree) should be (ops)
  }
  
  test("(A B) fragment") {
    val x = elABr
    
    expectResult( Cons(Cons(Car(x), nil), Cons(Cdr(x), nil)) ) {
      constructFragment(ellArlBrr, mapOfSubexpressions(elABr))
    }
  }
  
  test("predicate difference") {
    val x = ellABrlBlABrr
    val y = ellABrllABrlABrr
    
    val res = structureDifference(x, y)
    res.size should be (1)
    res(0)(nil) should be (Cdr(Car(nil)))
//    res(0)((ellABrlBlABrr)) should be (List(varB))
//    res(0)((ellABrllABrlABrr)) should be (List(ieLP_AB_RP))
  }

  test("compare trees") {
    implicit val map = Map[(Expr, Expr), Int]()

    def compRes(e1: Expr, e2: Expr) = compare(e1, e2)._1
    
    compRes(varA, varB) should be (0)
    compRes(varA, varA) should be (0)
    compRes(elABr, varA) should be (1)
    compRes(varA, elABr) should be (-1)
    compRes(varA, ellArlBrr) should be (-1)
    compRes(ellArlBrr, elABr) should be (1)
    compRes(ellABrllABrlABrr, ellABrlBlABrr) should be (1)
    compRes(ellABrlBlABrr, elBlABrlABrr) should be (0)
    compRes(elBlABrlABrr, ellABrlBlABrr) should be (0)
  }

  test("compare trees maps") {
    implicit var map = Map[(Expr, Expr), Int]()

    def compRes(e1: Expr, e2: Expr) = {
      assert ( compare(e1, e2)._2.size >= map.size)

      map = compare(e1, e2)._2
      compare(e1, e2)._1
    }
    
    compRes(varA, varB) should be (0)
    compRes(varA, varA) should be (0)
    compRes(elABr, varA) should be (1)
    compRes(varA, elABr) should be (-1)
    compRes(varA, ellArlBrr) should be (-1)
    compRes(ellArlBrr, elABr) should be (1)
    compRes(ellABrllABrlABrr, ellABrlBlABrr) should be (1)
    compRes(ellABrlBlABrr, elBlABrlABrr) should be (0)
    compRes(elBlABrlABrr, ellABrlBlABrr) should be (0)
  }

  test("sorting") {
    val inputExamples = List(nil, elAr, elABr, elABCr)
    
    
    for (permutation <- inputExamples.permutations) {
      assert(permutation.size == inputExamples.size)
      
      sort(permutation) should be (inputExamples)
    }
  }

}