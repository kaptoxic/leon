package leon
package test.ioexamples

import leon.synthesis.ioexamples._
import purescala._
import Expressions._

import org.scalatest.FunSuite
import org.scalatest._

class FragmenterTest extends FunSuite with Matchers {
  
  import ExampleInputs._
  import Extractors._
  import Util._
  import ExprOps._

  import Fragmenter._

  def assertExpressionMapping(e: Expr)(implicit map: Map[Expr, Expr]) = {
    assert(map contains e)
    map(e) should be (e)
  }
  
  test("test map of subexpressions") {
    // (A B)
    val in = elABr
    
    val subMap = mapOfSubexpressionsToPathFunctions(elABr)
    
    subMap.map({case (k, v) => (k, v(x)) }) should contain (elBr -> Cdr(x))
    
  }
  
  test("(A B) fragment") {
    // (A B)
    val in = elABr
    // ( (A) (B) )
    val output = ellArlBrr
    
    ellArlBrr should be (Cons(elAr, Cons(elBr, nil)))
    
    assertResult( Cons(Cons(Car(x), nil), Cons(Cdr(x), nil)) :: Nil ) {
      constructFragments((in :: Nil, ellArlBrr) :: Nil, x :: Nil)
    }
  } 
  
  test("fragments for unpack") {
    val iExamples = List(
      ieunpack1, ieunpack2, ieunpack3, ieunpack4  
    )
    val oExamples = List(
      oeunpack1, oeunpack2, oeunpack3, oeunpack4  
    )
    
    val givenFragments = List(
      fragUnpack1, fragUnpack2, fragUnpack3, fragUnpack4 
    )
        
    constructFragments((iExamples.map(_ :: Nil) zip oExamples), x :: Nil) should be (givenFragments)
  }
  
  test("ambigous fragments") {
    val iExamples = List(
      ieunpack1, ieunpack2, ieunpack3, ieunpack4  
    )
    val oExamples = List(
      oeunpack1, oeunpack2, oeunpack3, oeunpack4  
    )
    
    val givenFragments = List(
      fragUnpack1, fragUnpack2, fragUnpack3, fragUnpack4 
    )
        
    constructFragments((iExamples.map(_ :: Nil) zip oExamples), x :: Nil) should be (givenFragments)
  }
  
  // this is probably "last" example from ILP
//  import benchmarks._
//  
//  for ( bench <- List( last ) ) {
//    constructFragments(bench.ioPairs, x) should be (bench.fragments)
//  }

}