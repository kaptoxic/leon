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
  
  def ieLP_AB_RP = Cons(varA, Cons(varB, nil))
  
  // ((A)(B))
  def oeLP_AB_RP = Cons(Cons(varA, nil), Cons(Cons(varB, nil), nil))

}