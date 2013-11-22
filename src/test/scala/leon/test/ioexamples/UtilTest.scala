package leon
package test.ioexamples

import leon.synthesis.ioexamples._
import purescala.Trees._
import purescala._

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers._

// TODO codegen evaluator params should be used but not yet in master
class UtilTest extends FunSuite {
  
  import ExampleInputs._
  import Fragmenter._
  import Extractors._

  import Util._
  import TreeOps._

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
      map(subtree)(x) should be (ops)
  }
  
  test("(A B) map to subexpressions with substitutions") {
    
    val list = allSubexpressionsWithSubst(elABr)
        
    list.size should be (5)
    
    for((res, given) <- list zip List((elABr, w), (varA, Cons(w, Cons(varB, nil))),
      (Cons(varB, nil), Cons(varA, w)), (varB, Cons(varA, Cons(w, nil))), (nil, Cons(varA, Cons(varB, w))))) {
      res._1 should be (given._1)
      res._2(w) should be (given._2)
    }
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
    compRes(ellABrlBlABrr, elBlABrlABrr) should be (-2)
    compRes(elBlABrlABrr, ellABrlBlABrr) should be (-2)
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
    compRes(ellABrlBlABrr, elBlABrlABrr) should be (-2)
    compRes(elBlABrlABrr, ellABrlBlABrr) should be (-2)
  }

  test("sorting") {
    val inputExamples = List(nil, elAr, elABr, elABCr)
    
    
    for (permutation <- inputExamples.permutations) {
      assert(permutation.size == inputExamples.size)
      
      sort(permutation) should be (inputExamples)
    }
  }

}