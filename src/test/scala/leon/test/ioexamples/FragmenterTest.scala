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
    
    val subMap = mapOfSubexpressions(elABr)
    
    subMap.map({case (k, v) => (k, v(x)) }) should contain (elBr -> Cdr(x))
    
  }
  
  test("(A B) fragment") {
    // (A B)
    val in = elABr
    // ( (A) (B) )
    val output = ellArlBrr
    
    ellArlBrr should be (Cons(elAr, Cons(elBr, nil)))
    
    assertResult( Cons(Cons(Car(in), nil), Cons(Cdr(in), nil)) ) {
      constructFragment(ellArlBrr, mapOfSubexpressions(in).
        filterNot( _._1.isInstanceOf[NilList] ).mapValues(_(in)))
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
        
    constructFragments((iExamples zip oExamples), x) should be (givenFragments)
  }
  
  // this is probably "last" example from ILP
//  import benchmarks._
//  
//  for ( bench <- List( last ) ) {
//    constructFragments(bench.ioPairs, x) should be (bench.fragments)
//  }

}