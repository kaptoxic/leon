package leon
package scife.domain

import purescala.Common._
import purescala.Trees._
import purescala.TypeTrees._

trait Domain {
  
  def constraint(v: Variable): Expr

  def values: List[Expr]
  
  def tpe: TypeTree
  
//  def refine(d: Domain[T]): Domain[T]
  
}