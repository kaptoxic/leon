package leon
package synthesis
package ioexamples

import purescala._
import Expressions._
import ExprOps._
import Definitions._
import Common.Identifier

import leon.utils.logging.HasLogger

/**
 * @author ivcha
 * Extracts examples
 * NOTE: currently works with 1 input, 1 output
 */
class ExamplesExtraction(ctx: LeonContext, program: Program) extends HasLogger {
  
  type InputOutputExample = ((Identifier, Expr), (Identifier, Expr))
  
  def extract(problem: Problem): Seq[InputOutputExample] = {
    require(problem.as.size == 1)
    require(problem.xs.size == 1)
    info("extracting examples from problem: " + problem)
    
    val eFinder = new ExamplesFinder(ctx, program)

    val chooseEb = eFinder.extractFromProblem(problem)
    
    info(s"chooseEb.valids ${chooseEb.valids}")
    chooseEb.valids.collect({
      case InOutExample(in :: Nil, out :: Nil) =>
        ((problem.as.head, in), (problem.xs.head, out))
    })
  }
  
  def transformMappings(mappings: Seq[InputOutputExample]) = {
    val idsAndExamples =
      ((Set[(Identifier, Identifier)](), Set[(Expr, Expr)]()) /: mappings) {
      	case ((setIds, setIOs), ((inId, inE), (outId, outE))) =>
      	  ( setIds + ((inId, outId)), setIOs + ((inE, outE)) )
    	  case _ =>
    	    (Set.empty, Set.empty)
    	}
    
    if (idsAndExamples._1.size == 1) {
      Some((idsAndExamples._1.head, idsAndExamples._2.toList))
    } else None
  }
  
}