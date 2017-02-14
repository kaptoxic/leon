package leon
package test.ioexamples

import leon.synthesis.ioexamples._

import purescala._
import Expressions._

import org.scalatest.FunSuite
import org.scalatest._

// TODO codegen evaluator params should be used but not yet in master
class UtilTest extends FunSuite with Matchers {
  
  import ExampleInputs._
  import Fragmenter._
  import Extractors._

  import Util._
  import ExprOps._

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
    
    val map = mapOfSubexpressionsToPathFunctions(elABr)
        
    map.size should be (5)
    
    val x = elABr
    for((subtree, ops) <- List(
      (varA, Car(x)), (varB, Car(Cdr(x))), (Cons(varB, nil), Cdr(x)),
      (nil, Cdr(Cdr(x))), (elABr, x)
    ))
      map(subtree)(x) should be (ops)
  }
  
  test("(A B) map to subexpressions with substitutions") {
    
    val list = subexpressionsToContexts(elABr)
        
    list.size should be (5)
    
    list.map({ case (a, b) => (a, b(w)) }) should contain theSameElementsAs List(
      (elABr, w),
      (varA, Cons(w, Cons(varB, nil))), (Cons(varB, nil), Cons(varA, w)),
      (varB, Cons(varA, Cons(w, nil))), (nil, Cons(varA, Cons(varB, w)))
    )
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
  
  test("case classes (list type)") {
    
    import leon.test.condabd.util.Scaffold._
    import ExampleInputs._
    
    for (
          (sctx, f, p) <- forProgram(
            """
      	    import leon.lang.{ Map => _, _ }
            import leon.lang.synthesis._
            
            object Test {
            
              sealed abstract class List
              case class Cons(head: Int, tail: List) extends List
              case class Nil() extends List
              
              def lst() = Cons(0, Nil())
            
              def rec(in: List) = in match {
                case Nil() => Nil()
                case Cons(h, t) => choose {
                  (out : List) => true
                }
              }

            }
    	    """)
        ) {
    
      val program = sctx.program
  
      val consClass = program.caseClassDef("Cons").typed
      val nilClass = program.caseClassDef("Nil").typed
      val nilExp = CaseClass(nilClass, Nil): Expr
      
      val expr = CaseClass(consClass, IntLiteral(0) :: nilExp :: Nil)
      val list = mapOfSubexpressionsToPathFunctions(expr).map({ case(e, f) => (e, f(x))})
       
      withClue(list.mkString("\n")) {
        list.size should be (3)
        
        list.map(_._1) should contain (IntLiteral(0))
      }
      
      val expr2 = CaseClass(consClass, IntLiteral(1) :: expr :: Nil)
      val list2 = mapOfSubexpressionsToPathFunctions(expr2).map({ case(e, f) => (e, f(x))})
       
      withClue(list2.mkString("\n")) {
        list2.size should be (5)
        
        list2.map(_._1) should contain (IntLiteral(0))
      }

    }
  }

}