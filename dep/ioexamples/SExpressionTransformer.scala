package leon
package synthesis.ioexamples

import scala.collection.mutable.{ Map => MMap, TreeSet }

import leon.purescala.Definitions._
import leon.purescala.Trees._
import leon.purescala.TreeOps._

import insynth.util.logging.HasLogger

object SExpressionTransformer extends HasLogger {
  
  import Util._

  def transformToSExpression(e: Expr, p: Program): Option[Expr] = {
    
    p.definedClasses.
      collect({ case ccd: CaseClassDef if ccd.fields.size > 0 => ccd }) match {
      case (sClass@CaseClassDef(_, _, fields)) :: Nil if fields.size == 2 =>
        val sarResult = 
          searchAndReplace({
            case CaseClass(`sClass`, arg1 :: arg2 :: Nil) =>
              Some(Cons(arg1, arg2))
            // we do not care about Nil - we treat it same as atoms
            case _ => None
          }, true)(e)
          
        Some(sarResult)
      case _ =>
        warning("We can transform to S-expression only if one case class has two fields")
        None    
    }
  }

}