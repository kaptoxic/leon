package leon
package test.ioexamples

import purescala.Trees._
import purescala.TypeTrees._
import purescala.TreeOps
import purescala.Common.FreshIdentifier

import synthesis.ioexamples.Util

object ExampleInputs {
  
  import Util._
  
  val identifierA = FreshIdentifier("A")
  val identifierB = FreshIdentifier("B")
  val identifierC = FreshIdentifier("C")
  
  val varA = Variable(identifierA).setType(Int32Type)
  val varB = Variable(identifierB).setType(Int32Type)
  val varC = Variable(identifierC).setType(Int32Type)
  
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