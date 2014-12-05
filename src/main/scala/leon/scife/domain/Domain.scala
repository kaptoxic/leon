package leon
package scife.domain

import purescala.Common._
import purescala.Trees._

trait Domain[T] {
  
  def constraint(v: Variable): Expr

  def values: List[T]
  
}