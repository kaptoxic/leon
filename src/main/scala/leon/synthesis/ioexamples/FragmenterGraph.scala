package leon
package synthesis.ioexamples

import scala.collection.mutable.{ Map => MMap, TreeSet }

import purescala._
import Definitions._
import Types._
import Expressions._

import utils.logging.HasLogger
import leon.purescala.Common.FreshIdentifier

/*
 * Computes the decomposed input set
 * 
 * This algorithm corresponds to P2 for matching output because it tries to
 * match the topmost expression, if it cannot, it proceeds recursively (with searchAndReplace)
 */
class FragmenterGraph(
  program: Program,
  graphClass: ClassDef
  ) extends HasLogger {
  
  import Fragmenter._
  
  def constructFragment(example: IO, inputVariables: List[Variable]): Option[Expr] = {
    
    val (in :: Nil, out) = example
    
    example match {
      case (in :: Nil, out) if
        (graphClass.knownDescendants.map(_.typed) contains in.getType) &&
        (graphClass.knownDescendants.map(_.typed) contains out.getType) =>
          val diffs = Differencer.differenceConstraintsRelaxed(in, out)
          
          if (diffs.size == 1) {
            val (a, b) = diffs.head
            
            val standinId = FreshIdentifier("change", Untyped)
            val fragment = Hole(Untyped, standinId.toVariable :: a :: b :: Nil)
            
            Some(fragment)
          }
          else
            None
      case _ =>
        None
    }
    
  }
  
  // compute fragments for each given example pair
  def constructFragments(examples: List[IO], inputVariables: List[Variable]): List[Option[Expr]] = {
    // for each example
    for ((ie, oe) <- examples) yield {
      
      // map output examples and make input variable in place of placeholder
      constructFragment((ie, oe), inputVariables)
    }
  }
  
}