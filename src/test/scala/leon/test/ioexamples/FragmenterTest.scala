package leon
package test.ioexamples

import leon.synthesis.ioexamples._
import purescala.Trees._
import purescala._

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers._

class FragmenterTest extends FunSuite {
  
  import ExampleInputs._
  import Extractors._
  import Util._
  import TreeOps._

  import Fragmenter._

  def assertExpressionMapping(e: Expr)(implicit map: Map[Expr, Expr]) = {
    assert(map contains e)
    map(e) should be (e)
  }
  
  test("(A B) fragment") {
    val x = elABr
    
    expectResult( Cons(Cons(Car(x), nil), Cons(Cdr(x), nil)) ) {
      constructFragment(ellArlBrr, mapOfSubexpressions(elABr)
        .filterNot( _._1.isInstanceOf[NilList] ).mapValues(_(elABr)))
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

}