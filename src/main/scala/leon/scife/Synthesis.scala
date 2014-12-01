package leon.scife

import leon.purescala.Definitions._
import leon.purescala.Common._
import leon.purescala.TypeTrees._
import leon.purescala.Trees._
import leon.purescala.TreeOps._

import leon.synthesis.SynthesisContext
import leon.synthesis.Problem

import util._

object Synthesis {

  def apply(sctx: SynthesisContext, funDef: FunDef, problem: Problem) = 
    synthesize(sctx: SynthesisContext, funDef: FunDef, problem: Problem)
  
  def synthesize(sctx: SynthesisContext, funDef: FunDef, problem: Problem) = {
    val program = sctx.program
    val reporter = sctx.reporter
    
    // check synthesis problem
    // return type is Enum of some type
    
//    reporter info s"Owner of the function ${ funDef.owner.get }"
    val module = funDef.owner match {
      case Some(m: ModuleDef) => m
      case _ => throw new RuntimeException
    }
    assert(module.algebraicDataTypes.size == 1)
    
    val dataStructureDef = module.algebraicDataTypes.head
    val dataStructureType = classDefToClassType(dataStructureDef._1)
    
    val concreteForms = dataStructureDef._2
    assert(concreteForms.forall { classDef => !classDef.isAbstract && classDef.knownDescendents.isEmpty })
    
    val concreteLeafForms = concreteForms filter { classDef => classDef.isCaseObject || classDef.fields.isEmpty }
    
    assert(module.definedFunctions.find( _.id.name == "invariant" ).nonEmpty)
    val invariant = module.definedFunctions.find( _.id.name == "invariant" ).get
    assert(invariant.returnType == BooleanType)
    assert(invariant.hasBody)
    assert(invariant.params.size == 1)
    val invariantVar = invariant.params.head
    reporter info "Found an invariant"
    
    val termConstraintsMap = scala.collection.mutable.Map[Pattern, List[Constraint]]()
      .withDefaultValue(List.empty[Constraint])
    
    def collectVariables(ctx: Expr, mc: Pattern): Seq[(Identifier, Expr)] = mc match {
      case CaseClassPattern(bind, classType, subPatterns) =>
        assert( subPatterns.size == classType.classDef.fieldsIds.size)
        
        val nestedVariables =
          for ( (subPattern, field) <- (subPatterns zip classType.classDef.fieldsIds);
            vr <- collectVariables(CaseClassSelector(classType, ctx, field), subPattern)) yield vr
        
        reporter info s"bind empty? ${bind.isEmpty}"
        nestedVariables ++ bind.map( id => List( (id, ctx) )).getOrElse( Nil: List[(Identifier, Expr)] )
      case WildcardPattern(Some(id)) =>
        List( (id, ctx))
      case _: WildcardPattern => Nil
    }
    
    def getConstraints(funDef: TypedFunDef) = funDef.body.get match {
      case me@MatchExpr(_, s) =>
        assert(isMatchExhaustive(me))

        for(matchCase <- s) {
          matchCase match {
            case SimpleCase(CaseClassPattern(_, classType, Nil), rhs) if concreteLeafForms contains classType.classDef =>
              reporter info s"Found leaf form constraint $rhs for $classType"
            case SimpleCase(ccp@CaseClassPattern(_, classType, _), rhs) =>
              val matchedForm =
                concreteForms.find { cf => cf == classType.classDef } match {
                  case Some( res ) => res
                  case _ => throw new RuntimeException
                }
              val variables = collectVariables(funDef.params.head.toVariable, ccp).toMap
              reporter info s"Collected variables ${variables.mkString("\n")}"
              
              val recursiveAlgebraicFields =
                matchedForm.fields.filter { vd => vd.getType == dataStructureType }
              
              // check inductiveness of invariant
//              for (recField <- recursiveAlgebraicFields)
//                assert( exists({
//                          case FunctionInvocation(funDef, Variable(parameterId) :: Nil) =>
//                            variables.exists { case (id, assignedExpr) =>
//                              id == parameterId
//                            }
//                          case _ => false
//                        })( rhs ))
              
              def recognizeConstraint(expr: Expr) = expr match {
                case le@LessEquals(a, b) =>
                  termConstraintsMap(ccp) = Constraint(le, variables) :: termConstraintsMap(ccp)
                case _ => 
              }
              
              preTraversal(recognizeConstraint)(rhs)
              
              reporter info termConstraintsMap
          }          
        }
    }
    
    invariant.body.get match {
      case FunctionInvocation(funDef, params) =>
        assert(params.size == 1)
        assert(params.head.getType == dataStructureType)
        getConstraints(funDef)
      case _ =>
    }
    
  }

}