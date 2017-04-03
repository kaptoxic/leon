package leon
package synthesis.ioexamples

import scala.collection.mutable.{ Map => MMap, TreeSet }

import purescala._
import Types._
import Expressions._
import Constructors._
import Definitions._
import Common._

// evaluation
import evaluators._
import solvers._

import utils.logging.HasLogger

import scala.language.postfixOps

class Synthesizer extends HasLogger {

  type IO = (List[Expr], Expr)
  import Util._
  
  implicit var sortMap = MMap[IO, Int]()
  
  def synthesize(
    examples: List[InputOutputExample],
    getEnum: TypeTree => Iterable[Expr],
    evaluator: Evaluator,
    nilClass: ClassType,
    precalculatedFragmentsOpt: Option[List[Expr]] = None
  ): Option[(Expr, TypedFunDef)] = {
    require(examples.size > 0)

    val inIds =
      examples.head._1.map(_._1)
    val outId =
      examples.head._2._1
    require(examples.forall({
      case (inputs, output) =>
        inputs.map(_._1).toSet == inIds.toSet &&
        output._1 == outId
    }), "given examples should have a consistent input/output identifiers across examples")
    
    require({
      val usedIds = (Set[Identifier]() /: examples) {
        case (res, (_, (_, outExp))) =>
          res ++ ExprOps.collect({
            case x: Variable => Set(x.id)
            case _ => Set[Identifier]()
          })(outExp)
      }
      (usedIds diff inIds.toSet).isEmpty
    }, "given examples should use variables that are arguments to the function")

    /*
     * tranfsform from ([inputId], outId) -> ([inputValue], outVal) to
     *  [(inputId, inputVal)], (outId, outVal)
     */
    val ((inIds_check, outId_check), transformedExamples) =
      ExamplesExtraction.transformMappings(examples) match {
        case None =>
          throw new Exception("Examples do not match in terms of used variables")
        case Some(((inIds_check, outId_check), transformedExamples)) =>
          assert(inIds_check == inIds)
          assert(outId_check == outId)
          info(s"inIds $inIds")
          info("transformed examples: " + transformedExamples.mkString("\n"))
          ((inIds_check, outId_check), transformedExamples)
      }
    val inVars = inIds.map(_.toVariable)
    
    // calculate fragments (if not already given)
    // ambiguous -- fragments that can be explained by multiple expressions of the same size
    val (ambigous, unorderedFragments, unorderedFragmentsAll) =
      precalculatedFragmentsOpt match {
        // if I got fragments already, don't bother with ambiguity
        case Some(precalculatedFragments) =>
          (precalculatedFragments, Nil, precalculatedFragments)
          
        case None =>
          // get ambiguous fragments
          // TODO: this should be merged with previous function (no need to do work twice)
          val unorderedFragmentsAllAmbFlag = Fragmenter.checkAmbiguity(transformedExamples, inVars)
          fine("unorderedFragmentsAllAmbFlag: " + unorderedFragmentsAllAmbFlag.mkString("\n"))
      
          // construct fragments
          val unorderedFragmentsAll = Fragmenter.constructFragments(transformedExamples, inVars)
          assert(unorderedFragmentsAll.toSet.size == unorderedFragmentsAll.size)
          
          assert(unorderedFragmentsAllAmbFlag.size == unorderedFragmentsAll.size)
          val (ambigousPairs, unorderedFragmentsPairs) = (unorderedFragmentsAll zip unorderedFragmentsAllAmbFlag).
            partition( _._2._2 )
          val ambigous = ambigousPairs.map(_._1)
          val unorderedFragments = unorderedFragmentsPairs.map(_._1)
          info("unordered fragments: " + unorderedFragments.mkString("\n"))
          info("ambigous fragments: " + ambigous.mkString("\n"))
          
          assert((ambigous.toSet union unorderedFragments.toSet) == unorderedFragmentsAll.toSet)
          (ambigous, unorderedFragments, unorderedFragmentsAll)
      }
    fine("ambigous fragments: " + ambigous.mkString("\n"))
    fine("unordered fragments: " + unorderedFragments.mkString("\n"))
    
    val fragmentToInputMap =
      (Map[Expr, Set[InputOutputExampleVal]]() /: (unorderedFragmentsAll zip transformedExamples)) {
        case (current, (fragment, example)) =>
          current + (fragment -> (current.getOrElse(fragment, Set[InputOutputExampleVal]()) + example))
      }
    fine("fragmentToInputMap: " + fragmentToInputMap)
    
    // needed in the initial branches computation
    val transformedInputs = transformedExamples.map(_._1)
    var inputToFragmentMap = transformedInputs zip unorderedFragmentsAll toMap
    
    
    def fromExampleConditionToBranches(predicates: List[(List[List[Expr]], List[Expr])]) =
      for ( (examples, conditions) <- predicates) yield {
        
        val fragments = examples.map(inputToFragmentMap)
        info("fragments: " + fragments)
        
        if (fragments.distinct.size != 1) {
          throw new Exception
        }
            
        val branch =
          fragments.head
          
        (and(conditions.toSeq: _*), Left(branch))
      }
//    info("inital branches:\n" + initialBranches.map({ case (a, b) => a + "=>" + b }).mkString("\n"))
    
    // these predicates will tell us, for the given examples, this expression is *not* nil
    val (elseBranch, predicates) =
      // every input has different fragment 
      if (fragmentToInputMap.forall(_._2.size == 1) && unorderedFragments.size > 1) {
        // find differences by observing the inputs as a chain
        
        // initialFragmentsFromGroupPairs -- used to calculate initial predicates
        val (predicates, emptyDiffsFromCalculate, initialFragmentsFromGroup) =
          getChainedBranches(
            unorderedFragments: List[Expr],
            unorderedFragmentsAll: List[Expr],
            inVars: List[Variable],
            examples: List[InputOutputExample],
            getEnum: TypeTree => Iterable[Expr],
            evaluator: Evaluator,
            nilClass: ClassType
          )
        fine("predicates : " + predicates)
        fine("emptyDiffsFromCalculate: " + emptyDiffsFromCalculate.mkString("\n"))
        fine("initialFragmentsFromGroupPairs: " + initialFragmentsFromGroup.mkString("\n"))
        
        val emptyDiffs = ambigous ::: emptyDiffsFromCalculate

        val unhandledExamples = emptyDiffs.map(fragmentToInputMap(_).head)
        info("unhandled examples: " + unhandledExamples)
        
//        val (initialFragmentsFromGroup, initialFragmentsFromGroupTransformed) =
//          initialFragmentsFromGroupPairs.unzip
        
        val inputsForInitialFragments =
          initialFragmentsFromGroup.map(x => fragmentToInputMap(x).head)
        fine(initialFragmentsFromGroup.map(x => fragmentToInputMap(x)).toString)
        val unhandledInputs =
          (unhandledExamples ::: inputsForInitialFragments).map(_._1)
        info("unhandled inputs: " + unhandledInputs)
        ???
        
        val intialPredicatesIn =
          // drop last one since in includes one fragment from inputsForInitialFragments
          calculatePredicatesStructure(unhandledInputs, inVars).init
        fine("intialPredicatesIn:\n" + intialPredicatesIn)
          
        // all variables are Nil
        val fullConditioned = inVars
        
        // forgive me for doing this but I need this working
        // should overwrite inputps with modified fragment
        // TODO propagate the modified fragment in some better way
//        val mapOfInputsToNewFragments =
//          (inputsForInitialFragments.map(_._1) zip initialFragmentsFromGroupTransformed).toMap
//        fine("mapOfInputsToNewFragments: " + mapOfInputsToNewFragments.mkString("\n"))
//        fine("old inputToFragmentMap: " + inputToFragmentMap.mkString("\n"))

//        inputToFragmentMap =
//          inputToFragmentMap ++ mapOfInputsToNewFragments
       
        val initialPredicates =
          for ( (examples, predicates) <- intialPredicatesIn) yield { 
            val conditionsToRemove = 
              for ((x, predicate) <- predicates) yield predicate(x)
            val currentNils = fullConditioned.toSet -- conditionsToRemove
            val conditions =
              for (varToCheck <- currentNils) yield {
                IsInstanceOf(varToCheck, nilClass)
              }
            
            (examples, conditions.toList)
          }
        
        val (restPredicates, (elseBranchExprFun, elseArgMap)) = predicates
        
        (Some(elseBranchExprFun, elseArgMap), 
          fromExampleConditionToBranches(initialPredicates) ::: restPredicates)

      } else {
        // find predicates by identifying "selectors" that differentiate between inputs
        
        assert(inVars.size == 1)
        assert(fragmentToInputMap.size == 4, fragmentToInputMap.size)
        val predicates =
          calculatePredicatesStructureMapDifference(fragmentToInputMap.toSeq, inVars)
          
        val res =
          predicates map {
            case (k, v) =>
              val equalities = v.map {
                case (value, Not(expression)) =>
                  Not(Equals(value, expression))
                case (value, expression) =>
                  Equals(value, expression)
              }
              
              // FIXME change this not to be a function
//              (k.toList.map(_._1), Map((xs.head: Expr) -> ((_: Expr) => and(equalities.toSeq: _*))))
              (k.toList.map(_._1), equalities.toList)
          }
        
        (None, fromExampleConditionToBranches(res))
      }
    info("initialPredicates: " + predicates)//.map({ case (k,v) => (k, v.map(x => x._2(Util.w))) } ))

    // get type as type of output expression
    val resType = examples.head._2._2.getType
   
    // create a recursive function definition
    val newFun = new FunDef(
      FreshIdentifier("rec"),
      Nil,
      inVars map { v => ValDef(v.id) },
      resType
    ).typed
    
    val finalElseBranch =
      elseBranch match { 
        case Some((elseBranchExprFun, elseArgMap)) =>
          elseBranchExprFun(FunctionInvocation(newFun, inVars map elseArgMap))
        case None =>
          UnitLiteral()
      }
      
    // create final if-then-else expression by folding the "unfolded" fragments
    val ifExpr = 
      ((finalElseBranch: Expr) /: predicates.reverse) {
        case (current, (condition, Left(branch))) =>
          IfExpr(condition, branch, current) 
        case (current, (condition, Right((branchFun, argFun)))) =>
          IfExpr(condition, branchFun(FunctionInvocation(newFun, inVars map argFun)), current) 
      }
    
    // last if-then case
    newFun.fd.body = Some(ifExpr)
    
    Some((ifExpr, newFun))
    
//    // create final if-then-else expression by folding the "unfolded" fragments
//    val ifExpr = 
//      ((UnitLiteral(): Expr) /: restPredicates) {
//        case (current, (condition, branch)) =>
//          IfExpr(condition, branch, current) 
//      }
//
//    // last if-then case
//    newFun.fd.body = Some(ifExpr)
//    
//    Some((ifExpr, newFun))
//
//    if (predicates.size > 0) {
//      
//      for ((condition, f, argumentsMap) <- predicates) {
//      
//      
////      val recursiveFragmentTrue = {
////        val (fragmentsThatAreTrue, _) =
////          fragments.find(_._2).get
////        val (_, (mapB, a) :: Nil) =
////          filteredDiffGroups.find(_._1 == fragmentsThatAreTrue).get
////        a(FunctionInvocation(newFun, inVars map mapB))
////      }
////        
////      val recursiveFragmentFalse = {
////        val (fragmentsThatAreFalse, _) =
////          fragments.find(!_._2).get
////        val (_, (mapB, a) :: Nil) =
////          filteredDiffGroups.find(_._1 == fragmentsThatAreFalse).get
////        a(FunctionInvocation(newFun, inVars map mapB))
////      }
//      
//      val finalIfExpr = 
//        IfExpr(condition, recursiveFragmentTrue, recursiveFragmentFalse) 
//      
//      // create final if-then-else expression by folding the "unfolded" fragments
//      val ifExpr = 
//        ((finalIfExpr: Expr) /: initialBranches.reverse) {
//          case (current, (condition, branch)) =>
//            IfExpr(condition, branch, current) 
//        }
//      
//      // last if-then case
//      newFun.fd.body = Some(ifExpr)
//      
//      Some((ifExpr, newFun))
//      
//    } else if (initialBranches.size > 0) {
//      
//      // get type as type of output expression
//      val resType = examples.head._2._2.getType
//     
//      // create a recursive function definition
//      val newFun = new FunDef(
//        FreshIdentifier("rec"),
//        Nil,
//        inVars map { v => ValDef(v.id) },
//        resType
//      ).typed
//      
//      // create final if-then-else expression by folding the "unfolded" fragments
//      val ifExpr = 
//        ((UnitLiteral(): Expr) /: initialBranches.reverse) {
//          case (current, (condition, branch)) =>
//            IfExpr(condition, branch, current) 
//        }
//
//      // last if-then case
//      newFun.fd.body = Some(ifExpr)
//      
//      Some((ifExpr, newFun))
//    } else None
//    (fragments, predicates) match {
//      case (Some((fragments, a, b)), Some(p)) =>
//        if (fragments.size == p.size) {
//          // get type as type of output expression
//          val resType = examples.head._2.getType
//         
//          // create a recursive function definition
//          val newFun = new FunDef(
//            FreshIdentifier("rec"),
//            Nil,
//            ValDef(inputVariable.id) :: Nil,
//            resType
//          ).typed
//
//          assert(b.size == 1)
//          val recursiveFragment =
//            a(FunctionInvocation(newFun, Seq(b.head._2)))
//          
//          // last if-then case
//          newFun.fd.body = Some(
//            IfExpr(p.last, fragments.last, recursiveFragment)
//          )
//          
//          // create final if-then-else expression by folding the "unfolded" fragments
//          val ifExpr = 
//            ((p.init zip fragments.init) :\ (FunctionInvocation( newFun, Seq(inputVariable) ): Expr)) {
//              case ((pred, frag), elseExpr) =>
//                IfExpr(pred, frag, elseExpr) 
//            }
//          
//          Some((ifExpr, newFun))
//        } else
//          throw new Exception("Do not know how to combine fragments in this case.")
//      case _ =>
//        None
//    }
  }
  
  // currently just arbitrarily take one
  def choosePredicate(predicates: Iterable[(Expr, Iterable[(Set[(Expr, Expr)], Boolean)])]) =
    predicates.head
  
  def getChainedBranches(
    unorderedFragments: List[Expr],
    unorderedFragmentsAll: List[Expr],
    xs: List[Variable],
    examples: List[InputOutputExample],
    getEnum: TypeTree => Iterable[Expr],
    evaluator: Evaluator,
    nilClass: ClassType
  ) = {
    entering("getChainedBranches", unorderedFragments, unorderedFragmentsAll)

    // (fragments without found form, groups of fragments with common form)
    val (emptyDiffsFromCalculate, filteredDiffGroups) = calculateFragments(unorderedFragments, xs)
    fine("emptyDiffsFromCalculate:\n" + emptyDiffsFromCalculate.mkString("\n"))
    fine("filteredDiffGroups:\n" + filteredDiffGroups.mkString("\n"))
    
    // fragment that is the first within the group with common form  
    val initialFragmentsFromGroup =
      for ((startingFragments, finishingFragments) <- filteredDiffGroups.map(_._1.unzip)) yield {
        finest("startingFragmentsfinishingFragments: " + startingFragments)
        finest("finishingFragments: " + finishingFragments)
        val initialFragment =
          startingFragments.filterNot( finishingFragments contains _ )
        finest("initialFragment: " + initialFragment)
        val sorted = startingFragments.toList.sortBy(ExprOps.formulaSize(_))
        assert(sorted.count(x => ExprOps.formulaSize(sorted.head) == ExprOps.formulaSize(x)) == 1)
        sorted.head
      }
    fine("initialFragmentFromGroup: " + initialFragmentsFromGroup)

    val predicates =
      // if there is only one group of fragments with common form
      if (filteredDiffGroups.size == 1) {
        val (_, subs) = filteredDiffGroups.head
        
        // we don't know how to deal with this ambiguity though (should not happen?)
        assert(subs.size == 1)
        val (subMap, f) = subs.head
        
//        val singleGeneralizedFragment =
//          f(Hole(UnitType, xs.map(subMap)))
  
        (Nil, (f, subMap))
      } else {
        // found groups
        // TODO check if this is generalized to size >2
        // NOTE this will only partition the group into two
        val predicates =
          calculatePredicates(
            filteredDiffGroups,
            getEnum,
            unorderedFragmentsAll zip examples toMap,
            evaluator
          ) flatten

        fine("groups: " + filteredDiffGroups.mkString("\n"))
        fine("predicates: " + predicates.mkString("\n"))
          
        val chosenPredicate = choosePredicate(predicates)
        val (condition, fragmentsWithResult) = chosenPredicate
        
        val (fragmentsThatAreTrue, fragmentsThatAreFalse) = {
          val (fragmentsThatAreTrue, fragmentsThatAreFalse) =
            fragmentsWithResult.partition(_._2)

          (fragmentsThatAreTrue.head._1, fragmentsThatAreFalse.head._1)
        }
          
        def getExprAndArgumentMap(fragments: Set[(Expr, Expr)]) = {
          val (_, (mapB, a) :: Nil) =
            filteredDiffGroups.find(_._1 == fragments).get

          (a, mapB)
        }
          
        ((condition, Right(getExprAndArgumentMap(fragmentsThatAreTrue))) :: Nil,
          getExprAndArgumentMap(fragmentsThatAreFalse))
      }
    
    (predicates, emptyDiffsFromCalculate,
      initialFragmentsFromGroup.head :: Nil)
  }
  
  /*
   * returns:
   * - list of fragments for which common form was not identified
   * - list of n fragments (last n one), for which some common form was identified, as
   * -- (smaller, larger fragment) that share form
   * -- list of mappings and paths
   * --- mapping for substitution that would equate these fragments
   * --- path to the found form
   */
  def calculateFragments(unorderedFragments: List[Expr], xs: List[Variable]) = {
    entering("calculateFragments", unorderedFragments, xs)
    
    val fragments = Util.sort(unorderedFragments)
    info("fragments: " + fragments)
    
    val allDiffResults =
	    (for((f1, f2) <- fragments zip fragments.tail) yield {
	      val diffs = Differencer.differences(f1, f2, xs)
	      info(s"diffs for $f1 and $f2 are $diffs")
	      (f1, f2, diffs)
	    })
    info("allDiffResults: " + allDiffResults.map(_._3.map({ case (k, v) => (k, v(w))})))
    
    val (emptyDiffs, allDiffs) =
      allDiffResults.partition(_._3.isEmpty)
   
    // NOTE make this faster by doing equivalence classes
    val allPairsCompatibles =
      for ((f11, f21, diffs1) <- allDiffs;
        (f12, f22, diffs2) <- allDiffs.tail;
        // NOTE this can be optimized (not to check for all pairs)
        if f11 != f12;
        diff1 <- diffs1;
        diff2 <- diffs2;
        _ = finer(s"Checking diffs: ${diff1: String} and ${diff2: String}");
        merged <- Differencer.areCompatible(diff1, diff2);
        _ = finer(s"compatible!!")
      ) yield (Set((f11, f21), (f12, f22)), merged) 
      
    val compatibles =
      allPairsCompatibles.groupBy(_._2).toList map {
        case (commonDiffs, listOfCompatibleResults) =>
          (
            (Set[(Expr, Expr)]() /: listOfCompatibleResults) {
              case (current, (set, _)) =>
                current union set
            },
            commonDiffs
          )
      }
//    assert(compatibles == allPairsCompatibles, "just for merge -- remove me")

    // find groups for which we need to find distinguishing predicate
    val groups =
      compatibles.map({
        case (set, map) =>
          (set, map :: Nil)
      }) ++
      allDiffs.filter({
        case (f1, f2, diff) =>
          ! (compatibles.map(_._1).flatten contains (f1, f2))
      }).map({ case (f1, f2, diff) => (Set((f1, f2)), diff) })
      
    info(groups.toString)
    // FIXME hardcoded, but here we should check decreasing paramters
    // essentially, remove increasing recursive calls
    val filteredDiffGroups =
      groups.map({
        case (a, b) =>
          (a,
            b.filter(_.toString != "(Map(l2 -> l1, l1 -> Cons(l2.head, l1.tail)),<function1>)")
          )
      })
    info(filteredDiffGroups.mkString("\n"))
    
//    if (filteredDiffGroups.size < 2)
//      (emptyDiffs.map(_._1) ::: filteredDiffGroups, Nil)
//    else
      (emptyDiffs.map(_._1), filteredDiffGroups)
  }
  
  def calculatePredicates(
    filteredDiffGroups: List[(Set[(Expr, Expr)], Iterable[(Map[Variable, Expr], Expr => Expr)])],
    getEnum: TypeTree => Iterable[Expr],
    fragmentsAndInputsMap: Map[Expressions.Expr, InputOutputExample],
    evaluator: Evaluator
  ) = {
//    entering("calculatePredicates", inputExamplesList, xs)
    
    val enum = getEnum(BooleanType)
    val testedExpressions = enum.take(30)
    assert(testedExpressions.toList.distinct.size == testedExpressions.size)
    
    info("Evaluation")
    val distinguishing =
      for (conditionExpr <- testedExpressions;
        _ = info(s"example is $conditionExpr");
        (set, diffs) <- filteredDiffGroups;
        _ = assert(diffs.size == 1);
        (mapping, fun) = diffs.head
      ) yield {
        val res =
          for((_, f) <- set.toList;
            // note that the lowest fragment (out of two) might fail
//              if (compositeFragmentsAndInputsMap.contains(f));
            // NOTE we assume evaluation error actually is one of those simple cases that already work 
            (inputs, _) = fragmentsAndInputsMap(f)) yield {
            evaluator.eval(conditionExpr, new Model(inputs.toMap)) match {
              case EvaluationResults.Successful(BooleanLiteral(v)) =>
    //            print({if (v.asInstanceOf[BooleanLiteral].value) "t" else "f"})
                info(s"$v for $conditionExpr, and inputs ${inputs}")
                Some(v)
              case e: EvaluationResults.EvaluatorError =>
    //            print("_")
                info("evaluation failure: " + e + s" for inputs ${inputs}")
                None
            }
          }
          
        val allEqual = res
        info(s"results for $conditionExpr and $set: $allEqual")
        
        assert(allEqual.size > 0)

        if(
          allEqual.filterNot(_.isEmpty).distinct.size == 1
//            allEqual.filterNot(_.isEmpty).distinct.size ==
//            allEqual.filterNot(_.isEmpty).size
        )
          Some((conditionExpr, (set, allEqual.filterNot(_.isEmpty).head.get)))
        else
          None
      }
    
    info("distinguishing:\n" + distinguishing.flatten.mkString("\n")) 
    val distinguishedByGroup =
      distinguishing.flatten.groupBy(_._1).filter({
        case (k, res) if res.size == 2 =>
          val results = res.map(_._2._2)

          info(s"for $k we have:\n${results.mkString("\n")}")
          info(s"${results.toList.distinct}")
          results.toList.distinct.size == 2
        case _ =>
          false
      }).map({
        case (k, v) =>
          (k, v.map({ case (a, (b, c)) => (b, c)}))
      })
    info("distinguishing by group:\n" +distinguishedByGroup.mkString("\n")) 
    
    val results =
      for ((ex, setResults) <- distinguishedByGroup;
        _ = info(s"example is $ex");
        (set, diffs) <- filteredDiffGroups;
        _ = assert(diffs.size == 1);
        (mapping, fun) = diffs.head;
        modifiedEx = ExprOps.replaceFromIDs(mapping.map({ case (k,v) => (k.id, v) }), ex)
      ) yield {
        
        val res =
          for((chainedFragment, f) <- set.toList;
            // note that the lowest fragment (out of two) might fail
//              if (compositeFragmentsAndInputsMap.contains(f));
            // NOTE we assume evaluation error actually is one of those simple cases that already work 
            (inputs, _) = fragmentsAndInputsMap(f)) yield {
            evaluator.eval(modifiedEx, new Model(inputs.toMap)) match {
              case EvaluationResults.Successful(BooleanLiteral(v)) =>
    //            print({if (v.asInstanceOf[BooleanLiteral].value) "t" else "f"})
                info(s"$v for $modifiedEx, and inputs ${inputs}")
                val resultForChainedFragment =
                  setResults.find(_._1.map(_._2) contains chainedFragment)
                info(s"resultForChainedFragment: ${resultForChainedFragment}")
                if(resultForChainedFragment.isEmpty || v == resultForChainedFragment.get._2) {
                  true
                } else {
                  false
                }
              case e: EvaluationResults.EvaluatorError =>
    //            print("_")
                info("evaluation failure: " + e + s" for inputs ${inputs}")
                true
            }
          }
          
        val allEqual = res
        info(s"results for $ex and $set: $allEqual")
        
        assert(allEqual.size > 0)

        if(allEqual.forall(identity))
          Some((ex, setResults))
        else
          None
      }
    
    results
  }
   
  /*
   * list of predicates, where each predicate is for list of variables
   * each list represents examples for a variable
   * require: input examples are sorted
   */
  def calculatePredicatesStructure(inputExamplesVerbatim: List[List[Expr]], xs: List[Variable]) = {
    entering("calculatePredicatesStructure", inputExamplesVerbatim, xs) 
    
    // we need examples projected over each variable
    // given i1_x/y, i2_x/y get i1_x/i2_x, i1_y/i2_y
    assert(inputExamplesVerbatim.size >= 2)
    val inputExamplesList =
      (inputExamplesVerbatim.head.map(List(_)) /: inputExamplesVerbatim.tail) {
        case (soFarLists, elList) =>
          for ((currVarList, varInput) <- soFarLists zip elList) yield currVarList :+ varInput
      }
    info("unhandledInputsRightForm: " + inputExamplesList)

    // for each variable find predicates
    // (predicatesList, predicatesFunsList, partitions)
    val predicateGroups = (
      for ((inputExamples, x) <- inputExamplesList zip xs) yield {
        val atomExamples = inputExamples.map(substituteAllAtom)
        info("atomExamples: " + atomExamples)
      
        val predicatesFuns = Predicates.calculatePredicates(atomExamples, x)
        val predicates = predicatesFuns.map(_(x))
        info("predicates: " + predicates)

        // e.g. if we have Nil, Nil there will be no predicates
        if (predicates.size >= 1) {
          assert(predicates.size == 1) 
          
          // TODO sort in reverse order
          val partition = ((atomExamples zip inputExamples groupBy (_._1)).toList.sortBy(
            { p => -ExprOps.formulaSize(p._1) }).head)._2.map(_._2)
          
          Some((predicates.head, predicatesFuns.head, partition))
        }
        else None
      }
    )
    
    val examplesWithComponents =
      inputExamplesVerbatim.map({ e =>
        for ((ex, x) <- e zip xs) yield {
          ((e, x), ex) 
        }
      }) flatten
    val examplesWithComponentsMap = examplesWithComponents.toMap
    assert(examplesWithComponents.size == examplesWithComponentsMap.size)
    
    // return examples with predicates which are true for them
    // partition examples not their projection
    (((inputExamplesVerbatim, Map[Variable, Expr=>Expr]()) :: Nil) /: (predicateGroups zip xs)) {
      case ( (examplesWithPredicates, (Some((predicate, predicateFun, partitioned)), x)) ) =>
        val newResults =
          for ((examples, predicates) <- examplesWithPredicates) yield {
            def condition(ex: List[Expr]) =
              partitioned contains examplesWithComponentsMap((ex, x))
            fine("partitioned: " + partitioned)
            fine("examplesWithComponentsMap: " + examplesWithComponentsMap)
            
            val (yesPredicate, noPredicate) = examples.partition(condition)
            info(s"${(yesPredicate, noPredicate)} partition for $x")
            (yesPredicate, predicates + (x -> predicateFun)) ::
            (noPredicate, predicates) ::
            Nil
          }
        
        // FIXME SORT HERE Accordingly
        newResults.flatten.sortBy(_._2.size)
      case ( (examplesWithPredicates, (None, x)) ) =>
        examplesWithPredicates
    }
    
//    val (nums, diffSets, allDiffSets) = (
//      for ((predicates, x) <- predicatesList zip xs) yield {
//      
//        val allDiffs =
//    	    for((f1, f2) <- predicates zip predicates.tail) yield {
//    	      val diffs = Differencer.differences(f1, f2, x).map(_._1)
//    		    diffs.toSet
//    	    }
//        info("allDiffs: " + allDiffs)
//      
//        var flag = true
//        val (num, diffSet) = 
//    	    ((1, allDiffs.last) /: allDiffs.init.reverse) {
//    	      case ((num, diffSet), elSet) =>
//    	        // TEMP, assume one variable
//    	        assert(diffSet.size == 1)
//    	        if (flag && !(diffSet intersect elSet).isEmpty) (num+1, diffSet intersect elSet)
//    	        else {
//    	          flag = false
//    	          (num, diffSet)
//    	        }
//    	    }
//        info("(num, diffSet): " + (num, diffSet))
//        assert(diffSet.size == 1)
//        
//        (num, diffSet, allDiffs)
//      }
//    ).unzip3
//    info(s"(nums, diffSets, allDiffSets)=${(nums, diffSets, allDiffSets)}")
//
//    assert(nums.forall( _ == nums.head))
//    assert(diffSets.forall( _ == diffSets.head))
//    assert(allDiffSets.forall( _ == allDiffSets.head))
//    assert(predicatesFunsList.forall(_ == predicatesFunsList.head))
//    assert(predicatesList.forall(_ == predicatesList.head))
//    
//    val predicatesFunList = predicatesFunsList.head
//    val predicates = predicatesList.head
//    val num = nums.head
//    val allDiff = allDiffSets.head
//    val diffSet = diffSets.head
//    
//    // we check whether diffs entail same form for at least 2 consecutive examples
//    if (num >= 2) {
//      Some(predicates.dropRight(num) :+
//        predicatesFunList(allDiff.size - num)(diffSet.head.head._2))
//    }
//    else None
  }
  
  def calculatePredicatesStructureMapDifference(
    inputsPerPredicateMap: Seq[(Expr, Set[InputOutputExampleVal])],
    inputsVariables: List[Expr]
  ) = {
    
    // TODO at this point
    require(inputsVariables.size == 1)
    val inputVar = inputsVariables.head
    
    val (intersections, pairs) =
      (for ((fragment, pairs) <- inputsPerPredicateMap) yield {
        
        val subexpressionsSets =
          for ((inputs, _) <- pairs) yield         
            Util.subexpressionToPathFunctionsPairs(inputs.head).map({
              case (k, v) => (k, v(inputVar)) }).toSet
            
        info("subexpressionsSets:\n" + subexpressionsSets.mkString("\n"))
        
        val intersection =
          subexpressionsSets.reduce(_ intersect _)
          
        info("intersection:\n" + intersection.mkString("\n"))
        
        (intersection, pairs)
      }).unzip
  
    val sortedBySize =
      intersections.flatten.toList.distinct.sortBy(x => ExprOps.formulaSize(x._2))
        
    val partitions = (intersections, Set[(Expr, Expr)]()) :: Nil
    
    val newPartitions =
      (partitions /: sortedBySize) {
        case (current, expr) if current.size < intersections.size =>
          info("expression is: " + expr)
          val newPartitions =
            for ((partition, partitionSet) <- current) yield {
              val (have, dontHave) =
                partition.partition(p => p contains expr)
                
              if (have.isEmpty || dontHave.isEmpty)
                (partition, partitionSet) :: Nil
              else
                (have, partitionSet + expr) ::
                  (dontHave, partitionSet + ((expr._1, (Not(expr._2): Expr)))) :: Nil
            }
          
          info("newPartitions:\n" + newPartitions.flatten.mkString("\n"))
          info("*********")
          newPartitions.flatten
        case r =>
          r._1
      }
    
    info("newPartitions: " + newPartitions.map(_._2).mkString("\n"))
    
    // if properly partitioned
    if (newPartitions.forall(_._1.size == 1)) {
      val partitionToPairsMap = (intersections zip pairs).toMap
      
      newPartitions.map({ case (partitions, predicates) =>
        (partitionToPairsMap(partitions.head), predicates) })
    } else {
      Nil
    }
  }

}
