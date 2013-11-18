package leon
package synthesis.ioexamples

import purescala.{ Trees => ot }
import purescala.Trees.Expr
import purescala.TypeTrees._
import purescala.TreeOps

object Util {
  
  // avoid typing problems
  val listType = ListType(BooleanType)
  
  def Cons(h: Expr, t: Expr) = ot.Cons(h, t).setType(listType)
  def Nil = ot.NilList(listType)
  def Car(l: Expr) = ot.Car(l).setType(listType)
  def Cdr(l: Expr) = ot.Cdr(l).setType(listType)

}