package lesynth

import scala.util.Random

import insynth.structures.Weight._

import leon.purescala.Trees.{ Variable => LeonVariable, _ }
import leon.purescala.Common.Identifier

case class Evaluation(examples: IndexedSeq[Map[Identifier, Expr]], exampleFun: (Expr, Map[Identifier, Expr])=>Boolean,
  candidates: Array[(Expr, Weight)], exampleRunner: ExampleRunner) {
  
  val random: Random = new Random(System.currentTimeMillis)  
      
  // keep track of evaluations
  var nextExamples: Map[Int, Int] = Map() 
  
  var evaluations = Map[Int, Array[Boolean]]()
  
//  def evalAvailable(expr: Int) = {    
//    val nextExample = nextExamples.getOrElse(expr, 0)
//    if (nextExample >= examples.size) false
//    else true
//  }
  
  def evaluate(exprInd: Int) = {     
    numberOfEvaluationCalls += 1
    
    val nextExample = nextExamples.getOrElse(exprInd, 0)
    if (nextExample >= examples.size) throw new RuntimeException("Exhausted examples for " + exprInd)
    
    nextExamples += (exprInd -> (nextExample + 1))
        
    val example = examples(nextExample)
    val expressionToCheck = candidates(exprInd)._1
      
    val result = exampleFun(expressionToCheck, example)
    val evalArray = evaluations.getOrElse(exprInd, Array.ofDim[Boolean](examples.size))
    evalArray(nextExample) = result
    evaluations += (exprInd -> evalArray)
    result
  }
  
//  def evaluate(expr: Int, exampleInd: Int) = {
//    val nextExample = nextExamples.getOrElse(expr, 0)
//    assert(exampleInd <= nextExample)
//    
//    if (exampleInd >= nextExample) {
//	    nextExamples += (expr -> (nextExample + 1))	        
//	    val example = examples(nextExample)   
//	    val result = example(expr)
//	    val evalArray = evaluations.getOrElse(expr, Array.ofDim[Boolean](examples.size))
//	    evalArray(nextExample) = result
//	    evaluations += (expr -> evalArray)
//	    result   
//    } else {
//      assert(evaluations.contains(expr))
//      evaluations.get(expr).get(exampleInd)
//    }
//  }
  
  def evaluate(expression: Int, example: Int => Boolean) = {
    example(expression)
  }
  
  def getNumberOfExamples = examples.size

  var numberOfEvaluationCalls = 0
  def getEfficiencyRatio = numberOfEvaluationCalls.toFloat / (examples.size * evaluations.size)
    
  // candidate x <= candidate y heuristically (compare their sizes)
  def heurCompare(x: Int, y: Int) = candidates(x)._2 >= candidates(y)._2 
}