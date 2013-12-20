//package lesynth.examples
//
//import leon.purescala.Trees._
//import leon.purescala.Common._
//import leon.purescala.Definitions._
//import leon.purescala.TreeOps._
//
//import leon.synthesis._
//
//trait Example {
//  def getExpression(body: Expr, postcondition: Expr): Expr = BooleanLiteral(true)
//  
//  def getMapping: Map[Identifier, Expr]
//}
//
//case class InputExample(val input: Map[Identifier, Expr]) extends Example {
//  
//  override def getExpression(body: Expr, postcondition: Expr) = {
//    And(
//      super.getExpression(body, postcondition),
//      replace(Map(ResultVariable() -> body), matchToIfThenElse(postcondition))
//	)
//  }
//  
//  override def getMapping = input
//  
//}
//
//case class InputOutputExample(val input: Map[Identifier, Expr], val output: Expr) extends Example {
//  
//  override def getExpression(body: Expr, postcondition: Expr) = {
//    And(
//      super.getExpression(body, postcondition),
//      Equals(body, output)
//    )
//  }
//  
//  override def getMapping = input
//  
//}