package leon
package test.ioexamples

import leon.synthesis.ioexamples._

import purescala._
import Expressions._
import purescala.Definitions._

import org.scalatest.FunSuite
import org.scalatest.Matchers._

class SynthesizerTest extends FunSuite {
  
  import ExampleInputs._
  import Extractors._
  import Util._
  import ExprOps._

  test("predicate recursion on unpack") {
    val synthesizer = new Synthesizer

    val inputExamples = List(
      ieunpack1, ieunpack2, ieunpack3, ieunpack4
    ).map(substituteAllAtom)
    assert(sort(inputExamples) == inputExamples)
    
    synthesizer.calculatePredicates(inputExamples :: Nil, x :: Nil) should
    	be	(Some(List(x, Cdr(x))))
  }
  
  test("fragment recursion on unpack") {
    val synthesizer = new Synthesizer

    val iExamples = List(
      ieunpack1, ieunpack2, ieunpack3, ieunpack4  
    )
    val oExamples = List(
      oeunpack1, oeunpack2, oeunpack3, oeunpack4  
    )
    
    synthesizer.calculateFragments(iExamples map (List(_)) zip oExamples, x :: Nil) match {
      case Some((fragments, a, m)) => 
        m.size shouldBe 1
        val b = m.head._2
        fragments :+ a(b) should
          be (List(nil, Cons(x, nil), Cons(Cons(Car(x), nil), Cdr(x))))
      case _ => fail
    }
  }
  
//  test("synthesize unpack") {
//    val synthesizer = new Synthesizer
//
//    val iExamples = List(
//      ieunpack1, ieunpack2, ieunpack3, ieunpack4  
//    )
//    val oExamples = List(
//      oeunpack1, oeunpack2, oeunpack3, oeunpack4  
//    )
//    
//    val res = synthesizer.synthesize(iExamples zip oExamples)
//    
//    withClue(res) {
//      res match { 
//        case Some((IfExpr(x: Variable, nil, fi@FunctionInvocation(f1, args)), f2)) => 
//          assert(f1 == f2)
//          f2.body should be (Some(
//            IfExpr(Cdr(`x`), Cons(`x`, nil), Cons(Cons(Car(`x`), nil), 
//            FunctionInvocation(f2, Seq(Cdr(`x`)))))
//          ))
//        case _ =>
//          fail
//      }
//    }
//  }

}