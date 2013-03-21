package lesynth

import leon.purescala.Trees._
import leon.purescala.Definitions.{ Program, VarDecl, FunDef }
import leon.purescala.Common.{ Identifier, FreshIdentifier }
import leon.purescala.TreeOps
import leon.plugin.ExtractionPhase

import insynth.leon.loader.LeonLoader

class Refiner(program: Program, hole: Hole, holeFunDef: FunDef) {      
  import Globals._
  
  def isAvoidable(expr: Expr) =
    recurentExpression == expr || isCallAvoidableBySize(expr) || hasDoubleRecursion(expr)
  
  //val holeFunDef = Globals.holeFunDef
    
  val recurentExpression: Expr = 
	  if (holeFunDef.hasBody) {
	    FunctionInvocation(holeFunDef, holeFunDef.args map { varDecl => Variable(varDecl.id) })
	  } else
	    Error("Hole FunDef should have a body")
    
  // check according to size
  // true - YES, false - NO or don't know
  // basically a lexicographic (well-founded) ordering
  def isCallAvoidableBySize(expr: Expr) = {
	    	
    def isBadInvocation(expr2: Expr) = expr2 match {
	    case FunctionInvocation(`holeFunDef`, args) =>
	      (0 /: (args zip holeFunDef.args)) {
	        case (res, (arg, par)) if res == 0 => isLess(arg, par)
	        case (res, _) => res
	      } >= 0
	    case _ => false
	  }
	    
  	import TreeOps.treeCatamorphism
  	
  	treeCatamorphism(
	    isBadInvocation,
	    (b1: Boolean, b2: Boolean) => b1 || b2,
	    (t: Expr, b: Boolean) => b || isBadInvocation(t),
	    expr
    )  
  }
  
  def isLess(arg: Expr, variable: VarDecl): Int = {
	  def getSize(arg: Expr, size: Int): Int = arg match {
    	//case FunctionInvocation(`holeFunDef`, args) => 1
	    case CaseClassSelector(cas, expr, fieldId) if fieldId.name == "tail" => getSize(expr, size - 1)
	    case CaseClassSelector(cas, expr, fieldId) if fieldId.name == "head" => size + 1
	    case CaseClass(caseClassDef, head :: tail :: Nil) => getSize(tail, size + 1)
	    case CaseClass(caseClassDef, Nil) => 1
	    case v: Variable => if (v.id == variable.id) size else 1
	    case _ => //throw new RuntimeException("Could not match " + arg + " in getSize")
	      1
	  }
	  
	  getSize(arg, 0)
  }
  
  def hasDoubleRecursion(expr: Expr) = {      
    var found = false
    
  	def findRecursion(expr: Expr) = expr match {
	    case FunctionInvocation(`holeFunDef`, args) => true
	    case _ => false
	  }
    
  	def findRecursionInCall(expr: Expr, b: Boolean) = expr match {
	    case FunctionInvocation(`holeFunDef`, args) =>
	      if (b) found = true
	      true
	    case _ => b
	  }
  	
  	import TreeOps.treeCatamorphism
  	
  	treeCatamorphism(findRecursion, (b1: Boolean, b2: Boolean) => b1 || b2, findRecursionInCall, expr)
  	
  	found
  }
  
}