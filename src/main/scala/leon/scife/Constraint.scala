package leon
package scife

import scala.collection.mutable.{ Map => MMap }

import leon.purescala._
import Trees._
import Definitions._
import Common._

case class Constraint(val expr: Expr, val variableInterpretations: Map[Identifier, Expr]) {
  
  val variableConstraints = MMap[Identifier, Set[Expr]]()
  
  def variables: Iterable[Identifier] = variableInterpretations.keys
  
  def constrainVariable(v: Identifier, e: Expr) =
    variableConstraints(v) += e    
    
}