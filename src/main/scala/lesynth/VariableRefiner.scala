package lesynth

import scala.collection.mutable.{Map => MutableMap}
import scala.collection.mutable.{Set => MutableSet}

import leon._
import leon.purescala.Trees._
import leon.purescala.TypeTrees._
import leon.purescala.Definitions.{ Program, VarDecl, FunDef, VarDecls }
import leon.purescala.Common.{ Identifier, FreshIdentifier }

import insynth.interfaces._
import insynth.leon.loader._
import insynth.leon._

import insynth.util.logging.HasLogger

// each variable of super type can actually have a subtype
// get sine declaration maps to be able to refine them  
class VariableRefiner(directSubclassMap: Map[ClassType, Set[ClassType]], variableDeclarations: Seq[LeonDeclaration],
  classMap: Map[Identifier, ClassType], reporter: Reporter = new DefaultReporter) extends HasLogger {  
  
    // map from identifier into a set of possible subclasses
  private var variableRefinements: MutableMap[Identifier, MutableSet[ClassType]] = MutableMap.empty
    for (varDec <- variableDeclarations) {
      varDec match {
        case LeonDeclaration(_, _, typeOfVar: ClassType, ImmediateExpression(_, Variable(id))) =>
          variableRefinements += (id -> MutableSet(directSubclassMap(typeOfVar).toList: _*))
        case _ =>
      }
    }    
  
  def checkRefinements(expr: Expr, allDeclarations: List[Declaration]) =
	  // check for refinements
	  getIdAndClassDef(expr) match {
	    case Some(refinementPair @ (id, classType)) =>
	      fine("And now we have refinement type: " + refinementPair)
	      fine("variableRefinements(id) before" + variableRefinements(id))
	      variableRefinements(id) -= classMap(classType.id)
	      fine("variableRefinements(id) after" + variableRefinements(id))
	
	      // if we have a single subclass possible to refine
	      if (variableRefinements(id).size == 1) {
	        reporter.info("We do variable refinement for " + id)
	
	        val newType = variableRefinements(id).head
	        fine("new type is: " + newType)
	
	        // update declarations
	        val newDeclarations =
	          for (dec <- allDeclarations)
	            yield dec match {
	            case LeonDeclaration(inSynthType, _, decClassType, imex @ ImmediateExpression(_, Variable(`id`))) =>
	              LeonDeclaration(
	                imex, TypeTransformer(newType), newType)
	            case _ =>
	              dec
	          }
	        
	        (true, newDeclarations)	
	      } else {
	        fine("we cannot do variable refinement :(")
	        (false, allDeclarations)
	      }
	    case _ =>
        (false, allDeclarations)
	  }

  // inspect the expression if some refinements can be done
  def getIdAndClassDef(expr: Expr) = expr match {
    case CaseClassInstanceOf(classDef, Variable(id)) =>
      Some((id, classDef))
    case _ =>
      None
  }
  
}