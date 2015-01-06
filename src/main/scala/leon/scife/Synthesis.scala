package leon.scife

import leon.purescala.Definitions._
import leon.purescala.Common._
import leon.purescala.TypeTrees._
import leon.purescala.Trees._
import leon.purescala.TreeOps._

import leon.synthesis.SynthesisContext
import leon.synthesis.Problem

import util._

import scala.language.postfixOps

object Synthesis {

  def apply(sctx: SynthesisContext, funDef: FunDef, problem: Problem) = 
    synthesize(sctx: SynthesisContext, funDef: FunDef, problem: Problem)
  
  // recognizes the "invariant" function and synthesizes generator that satisfies
  // the invariant
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
    
    // forms to enumerate
    val concreteForms = dataStructureDef._2
    assert(concreteForms.forall { classDef => !classDef.isAbstract && classDef.knownDescendents.isEmpty })
    
    // Nil vs Cons
    val (concreteSimpleForms, concreteComplexForms) =
      concreteForms partition {
        classDef => classDef.isCaseObject || classDef.fields.isEmpty
      }
    
    // invariant
    assert(module.definedFunctions.find( _.id.name == "invariant" ).nonEmpty)
    val invariant = module.definedFunctions.find( _.id.name == "invariant" ).get
    assert(invariant.returnType == BooleanType)
    assert(invariant.hasBody)
    assert(invariant.params.size == 1)
    val invariantVar = invariant.params.head
    reporter info "Found an invariant"
    
    // list of pairs (id, expression to that variable in the invariant)
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
    
    // constraints
    // list of constraints for particular patterns
    val termConstraintsMap = scala.collection.mutable.Map[Pattern, List[Constraint]]()
      .withDefaultValue(List.empty[Constraint])

    // extract constraints
    def getConstraints(funDef: TypedFunDef) = funDef.body.get match {
      case me@MatchExpr(_, s) =>
        assert(isMatchExhaustive(me))

        for(matchCase <- s) {
          matchCase match {
            case SimpleCase(CaseClassPattern(_, classType, Nil), rhs) if concreteSimpleForms contains classType.classDef =>
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
    
    //
    reporter info termConstraintsMap
    
    
    // expressions to enumerate, on invariant var
    // for Cons form
    for (concreteComplexForm <- concreteComplexForms) {
      val concreteComplexFormType = classDefToClassType(concreteComplexForm).
        asInstanceOf[CaseClassType]
      val exprToSynthesize = concreteComplexForm.fieldsIds map {
        fieldId => CaseClassSelector(concreteComplexFormType, invariantVar.toVariable, fieldId)
      }
      
      import domain._
      
      val sizeFunDef = module.definedFunctions.find
        { funDef => funDef.id.name == "size" }.get.typed
      
      // TODO HARDCODED
      val initialDomains = Map[Identifier, Domain](
        concreteComplexForm.fieldsIds.find { _.name == "head" }.get -> Range(5 to 10)
      ) 
      val sizeDomain = sizeFunDef.id -> IntegerVal(5)

      // HARDCODED size constraint
      termConstraintsMap += (
        CaseClassPattern(None, concreteComplexFormType, Nil) ->
        List(Constraint(
          FunctionInvocation(sizeFunDef, List(invariantVar.toVariable)), Map()
        ))
      )
      
      val unknowns = concreteComplexFormType.fields flatMap {
        fld => fld.getType match {
          case ct: ClassType if ct == dataStructureType =>
            (initialDomains.toList map {
              id => (fld.id.name + "." + id._1, id._2)
            }) ::: List( (fld.id.name + ".size", Unknown) )
            // size domain
          case BooleanType | Int32Type | CharType =>
            (fld.id.name, fld.getType) :: Nil
        }
      }
      reporter info unknowns

//      case class Solver(unknowns: List[(Identifier, TypeTree)],
//        initialDomains: Map[Identifier, Domain], constraints: List[Constraint]) {
//        
//        var solved = List[(Identifier, Domain)]()
//        
//      } map { fld => (fld.id.fullName, initialDomains(fld.id.fullName)) } toList
//      
//      val domains = ("size", initialDomains("size")) :: fieldDomains
      
    }

  }

}