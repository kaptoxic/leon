package leon
package synthesis.ioexamples
package backwards

import scala.collection.{ mutable => m }

import purescala.Trees._
import purescala.TypeTrees._
import purescala._
import purescala.Definitions._
import purescala.Common._
import evaluators._

import insynth.util.logging.HasLogger

class Synthesizer(evaluator: Evaluator) extends HasLogger {

  type IO = (Expr, Expr)
  import Util._
  
  implicit var sortMap = m.Map[IO, Int]()
  
  def evaluate(expr: Expr, mapping: Map[Identifier, Expr]) = {
    import EvaluationResults._
    
    val res =
      evaluator.eval(expr, mapping) match {
        case Successful(res) =>
          res
        case m =>
          throw new Exception("Evaluation problem")
      }
    info("Eval res is: " + res)
    
    res
  }
  
  def synthesize(
    inputVar: Identifier,
    examples: List[IO],
    functions: Map[TypeTree, Seq[FunDef]],
    decomposer: Expr => Map[Expr, Expr]) = {
    
    var branches = new m.LinkedList[Branch]
    
    case class Branch(condition: Expr) {
      
      var examples = new m.LinkedList[IO]
      
      var nodes = m.Queue[Node]()
      
      def getBody = {
        if (nodes.head.resolved) Some(nodes.head.expr)
        else None
      }
      
      def search: Boolean = {        
        var found = false
        
        while (!found && !nodes.isEmpty) {
          val headNode = nodes.head
          
          found =
            examples.forall( ex =>
              evaluate(headNode.expr, Map(inputVar -> ex._1)) == ex._2
            )
            
          headNode.resolved = found
        }
        
        found
      }
    }
    
    case class Node(expr: Expr, inverses: List[Expr]) {
      var resolved = false
    }
      
    val headExample = examples.head
    
    val branch = Branch(BooleanLiteral(true))
    branch.examples :+= headExample
    branch.nodes enqueue Node(inputVar.toVariable, List(inputVar.toVariable))
    
    branches :+= branch
    
    branch.search
    
    branch.getBody
  }
}