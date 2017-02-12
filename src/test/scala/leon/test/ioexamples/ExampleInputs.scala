package leon
package test.ioexamples

import purescala._
import Expressions._
import Definitions._
import Types._
import purescala.Common.FreshIdentifier

import synthesis.ioexamples.Util

object ExampleInputs {
  
  import Util._
  
  val identifierA = FreshIdentifier("A", Int32Type)
  val identifierB = FreshIdentifier("B", Int32Type)
  val identifierC = FreshIdentifier("C", Int32Type)
  val identifierX = FreshIdentifier("x", Int32Type)
  
  val varA = Variable(identifierA)
  val varB = Variable(identifierB)
  val varC = Variable(identifierC)
  val x = Variable(identifierX)
  
  // ()
  val elr = nil
  // (A)
  val elAr = Cons(varA, nil)
  // (B)
  val elBr = Cons(varB, nil)
  // (C)
  val elCr = Cons(varC, nil)  
  // (A B)
  val elABr = Cons(varA, elBr)
  // (A B C)
  val elABCr = Cons(varA, Cons(varB, elCr))
  
  // ((A)(B))
  val ellArlBrr = Cons(Cons(varA, nil), Cons(Cons(varB, nil), nil))
  
  // (B (A B))
  val elBlABrr  = Cons(varB, elABr)
  // ((A B)(B (A B))
  val ellABrlBlABrr  = Cons(elABr, elBlABrr)
  // ((A B)((A B) (A B))
  val ellABrllABrlABrr  = Cons(elABr, Cons(elABr, elABr))
  // ((B (A B))(A B))
  val elBlABrlABrr = Cons(elBlABrr, elABr)
  
  // (w)
  val elWr = Cons(w, nil)
  // (w w)
  val elWWr = Cons(w, elWr)
  // (w w w)
  val elWWWr = Cons(w, elWWr)

  // (w.w)
  val elWdWr = Cons(w, w)
  // ((w.w).w)
  val ellWdWrdWr = Cons(elWdWr, w)
  // (((w.w).w).w)
  val elllWdWrdWrdWr = Cons(ellWdWrdWr, w)
  
  // examples for the unpack benchmark
  // inputs
  val ieunpack1 = nil
  val ieunpack2 = elAr
  val ieunpack3 = elABr
  val ieunpack4 = elABCr

  // outputs
  val oeunpack1 = nil
  val oeunpack2 = Cons(elAr, nil)
  val oeunpack3 = Cons(elAr, Cons(elBr, nil))
  val oeunpack4 = Cons(elAr, Cons(elBr, Cons(elCr, nil)))
  
  val fragUnpack1 = nil
  val fragUnpack2 = Cons(x, nil)
  val fragUnpack3 = Cons(Cons(Car(x), nil), Cons(Cdr(x), nil))
  val fragUnpack4 = Cons(Cons(Car(x), nil), Cons(Cons(Car(Cdr(x)), nil), Cons(Cdr(Cdr(x)), nil)))
  
  // predicates
  val predUnpack1 = w
  val predUnpack2 = Cdr(w)
  val predUnpack3 = Cdr(Cdr(w))
}

object ExampleInputsCaseClass {
  
  import leon.test.condabd.util.Scaffold._

  val (ctx, program) = forProgramDefinitions(
    """
    import leon.lang._
    import leon.lang.synthesis._
  
    object Tree {
      sealed abstract class Tree
      case class Node(left: Tree, right: Tree) extends Tree
      case object Nil extends Tree
    }
    """
  )

  val consClass = program.caseClassDef("Node").typed
  val nilClass = program.caseClassDef("Nil").typed
  val nil = CaseClass(nilClass, Nil): Expr
  
  val l = Variable(FreshIdentifier("l", consClass))
  val l1 = Variable(FreshIdentifier("l1", consClass))
  val l2 = Variable(FreshIdentifier("l2", consClass))
  val l3 = Variable(FreshIdentifier("l3", consClass))
  
  val identifierA = FreshIdentifier("A", Int32Type)
  val identifierB = FreshIdentifier("B", Int32Type)
  val identifierC = FreshIdentifier("C", Int32Type)
  val identifierX = FreshIdentifier("x", Int32Type)
  
  val varA = Variable(identifierA)
  val varB = Variable(identifierB)
  val varC = Variable(identifierC)
  val x = Variable(identifierX)
  val w = Variable(FreshIdentifier("w", consClass))
  
  def Cons(expr: Expr, tail: Expr) =
    CaseClass(consClass, expr :: tail :: Nil)
  
  // ()
  val elr = nil
  // (A)
  val elAr = Cons(varA, nil)
  // (B)
  val elBr = Cons(varB, nil)
  // (C)
  val elCr = Cons(varC, nil)  
  // (A B)
  val elABr = Cons(varA, elBr)
  // (A B C)
  val elABCr = Cons(varA, Cons(varB, elCr))
  
  // ((A)(B))
  val ellArlBrr = Cons(Cons(varA, nil), Cons(Cons(varB, nil), nil))
  
  // (B (A B))
  val elBlABrr  = Cons(varB, elABr)
  // ((A B)(B (A B))
  val ellABrlBlABrr  = Cons(elABr, elBlABrr)
  // ((A B)((A B) (A B))
  val ellABrllABrlABrr  = Cons(elABr, Cons(elABr, elABr))
  // ((B (A B))(A B))
  val elBlABrlABrr = Cons(elBlABrr, elABr)
  
  // (w)
  val elWr = Cons(w, nil)
  // (w w)
  val elWWr = Cons(w, elWr)
  // (w w w)
  val elWWWr = Cons(w, elWWr)

  // (w.w)
  val elWdWr = Cons(w, w)
  // ((w.w).w)
  val ellWdWrdWr = Cons(elWdWr, w)
  // (((w.w).w).w)
  val elllWdWrdWrdWr = Cons(ellWdWrdWr, w)
  
}