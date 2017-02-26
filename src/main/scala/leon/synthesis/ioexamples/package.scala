package leon
package synthesis

import purescala._
import Expressions._
import ExprOps._
import Definitions._
import Common.Identifier

package object ioexamples {
  
  type InputOutputExample = (List[(Identifier, Expr)], (Identifier, Expr))
  
  
  type InputOutputExampleVal = (List[Expr], Expr)
  
}