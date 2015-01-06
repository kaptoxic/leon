package leon
package scife.domain

import purescala.Common._
import purescala.Trees._
import purescala.TypeTrees._

case object Unknown extends Domain {
  
  override val tpe = Untyped
  
  def constraint(v: Variable) =
      throw new RuntimeException

  def values = List()

}