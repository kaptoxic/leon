package leon
package test.ioexamples

import leon.synthesis.ioexamples._

import purescala._
import Expressions._

import org.scalatest.FunSuite
import org.scalatest.Matchers._

class PredicatesTest extends FunSuite {
  
  import Extractors._
  import Util._
  import ExprOps._
  
  import Predicates._

  {
    import ExampleInputs._
  
    test("predicate difference") {
      val x = ellABrlBlABrr
      val y = ellABrllABrlABrr
      
      val res = structureDifference(x, y)
      res.size should be (1)
      res(0)(nil) should be (Cdr(Car(nil)))
    }
  
    test("find chain") {
      val inputExamples = List(elAr, elABCr).map( substituteAllAtom )
      val correctChain = List(List(elWWr, elWWWr).map (substituteAllAtom))
      
      // ensure all input examples have atoms replaced with w
      for (ex <- inputExamples) {
        val gathered = (List[Expr]() /: allSubexpressions(ex)) {
          (res, el) => res ++ collect({
            case Atom(e) => Set(e)
            case _ => Set[Expr]()
          })(el)
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

  {
    val exampleInputs = ExampleInputsCaseClass
    import exampleInputs._
    
    import Constructors._
    import UtilCaseClass._
    
    implicit val program = exampleInputs.program
    
    test("predicate difference, case class") {
      val x = ellABrlBlABrr
      val y = ellABrllABrlABrr

      val list = CaseClass(consClass, CaseClass(consClass, l1 :: l2 :: Nil) :: l3 :: Nil)
      
      val res = structureDifference(x, y)
      res.size should be (1)
      res(0)(list) should be (caseClassField(caseClassField(list, "left"), "right"))
    }
    
    test("find chain w -> (w (((w w) w) w)), case class") {
      val inputExamples = List(w, elllWdWrdWrdWr)
      val correctChain = List(List(elWdWr, ellWdWrdWr, elllWdWrdWrdWr))
      
      val res = findChain(inputExamples)
      res.size should be (1)
      res should be (correctChain)
    }
    
    test("find chain (... w) -> (... (((w w) w) w)), case class") {
      val inputExamples = List(w, elllWdWrdWrdWr)
      val correctChain = List(elWdWr, ellWdWrdWr, elllWdWrdWrdWr)
      
      for(leftExpr <- List(elWdWr, ellWdWrdWr, elllWdWrdWrdWr,
        elBlABrr, ellABrlBlABrr, ellABrllABrlABrr, elBlABrlABrr).map(substituteAllAtomWith(w))) {      
  	    val res = findChain(inputExamples map { Cons(leftExpr, _) })
  	    res.size should be (1)
  	    res.head.map(t => ExprOps.postMap({
  	      case v: Variable if v.id.name == "w" => Some(w)
  	      case _ => None
  	    }, false)(t)) should be (correctChain map { Cons(leftExpr, _) })
      }
    }
    
  }

}