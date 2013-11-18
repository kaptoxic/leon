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
    
    val map = allSubexpressions(ieLP_AB_RP)
        
    map.size should be (5)
    
    for(subexp <- List(varA, varB, Cons(varB, nil), nil, ieLP_AB_RP))
      map contains subexp
  }
  
  test("(A B) map to subexpressions") {
    
    val map = mapOfSubexpressions(ieLP_AB_RP)
        
    map.size should be (5)
    
    val x = ieLP_AB_RP
    for((subtree, ops) <- List(
      (varA, Car(x)), (varB, Car(Cdr(x))), (Cons(varB, nil), Cdr(x)),
      (nil, Cdr(Cdr(x))), (ieLP_AB_RP, x)
    ))
      map(subtree) should be (ops)
  }
  
  test("(A B) fragment") {
    val x = ieLP_AB_RP
    
    expectResult( Cons(Cons(Car(x), nil), Cons(Cdr(x), nil)) ) {
      constructFragment(oeLP_AB_RP, mapOfSubexpressions(ieLP_AB_RP))
    }
  }

}