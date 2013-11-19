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
  
  val nil = NilList(Int32Type).setType(ListType(Int32Type))
  val varA = Variable(identifierA).setType(Int32Type)
  val varB = Variable(identifierB).setType(Int32Type)
  val varC = Variable(identifierC).setType(Int32Type)
  
  // ()
  def elr = nil
  // (A)
  def elAr = Cons(varA, nil)
  // (B)
  def elBr = Cons(varB, nil)
  // (C)
  def elCr = Cons(varC, nil)  
  // (A B)
  def elABr = Cons(varA, elBr)
  // (A B C)
  def elABCr = Cons(varA, Cons(varB, elCr))
  
  // ((A)(B))
  def ellArlBrr = Cons(Cons(varA, nil), Cons(Cons(varB, nil), nil))
  
  // (B (A B))
  def elBlABrr  = Cons(varB, elABr)
  // ((A B)(B (A B))
  def ellABrlBlABrr  = Cons(elABr, elBlABrr)
  // ((A B)((A B) (A B))
  def ellABrllABrlABrr  = Cons(elABr, Cons(elABr, elABr))
  // ((B (A B))(A B))
  def elBlABrlABrr = Cons(elBlABrr, elABr)

}