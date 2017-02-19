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

class Synthesizer extends HasLogger {

  type IO = (List[Expr], Expr)
  import Util._
  
  implicit var sortMap = MMap[IO, Int]()
  
  def synthesize(
    examples: List[InputOutputExample],
    getEnum: TypeTree => Iterable[Expr],
    evaluator: Evaluator,
    nilClass: ClassType
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
    }))
    
    require({
      val usedIds = (Set[Identifier]() /: examples) {
        case (res, (_, (_, outExp))) =>
          res ++ ExprOps.collect({
            case x: Variable => Set(x.id)
            case _ => Set[Identifier]()
          })(outExp)
      }
      (usedIds diff inIds.toSet).isEmpty
    })

    val transformed = ExamplesExtraction.transformMappings(examples)
    if (transformed.isEmpty)
      throw new Exception("Examples do not match in terms of used variables")

    val ((inIds_check, outId_check), transformedExamples) = transformed.get
    assert(inIds_check == inIds)
    assert(outId_check == outId)
    info(s"inIds $inIds")
    info("transformed examples: " +transformedExamples.mkString("\n"))
    
    val xs = inIds.map(_.toVariable)
//    
//    // XXX -- hack -- make sort do equivalence classes
//    val unorderedFragments = unorderedFragmentsAll.filter(_.toString != "Nil")
//    info("unordered fragments: " + unorderedFragments.mkString("\n"))
//    
//    val fragments = Util.sort(unorderedFragments)
//    info("fragments: " + fragments)

    val (emptyDiffs, filteredDiffGroups) = calculateFragments(transformedExamples, xs)
//    assert(emptyDiffs.size == 1)
    info(filteredDiffGroups.mkString("\n"))
    assert(filteredDiffGroups.size == 2)
    
    // get fragments
    val unorderedFragmentsAll = Fragmenter.constructFragments(transformedExamples, xs)
    info("unorderedFragmentsAll: " + unorderedFragmentsAll)

    assert(unorderedFragmentsAll.toSet.size == unorderedFragmentsAll.size)
    val fragmentToInputMap = unorderedFragmentsAll zip transformedExamples toMap

    fine("fragmentToInputMap: " + fragmentToInputMap)
    
//    val predicates =
//      calculatePredicates(
//        filteredDiffGroups,
//        getEnum,
//        fragmentToInputMap,
//        evaluator
//      )
//      
//    fine("groups: " + filteredDiffGroups.mkString("\n"))
//    fine("predicates: " + predicates.mkString("\n"))
    
    val unhandledExamples = emptyDiffs.map(fragmentToInputMap(_))
    val initialFragmentFromGroup = {
      // get all (f1, f2)
      val (startingFragments, finishingFragments) =
        filteredDiffGroups.map(_._1).flatten.unzip
      fine("(startingFragments, finishingFragments): " + (startingFragments, finishingFragments))
      val initialFragment =
        startingFragments.filterNot( finishingFragments contains _ )
      fine("initialFragment: " + initialFragment)
      assert(initialFragment.size == 1)
      initialFragment.head
    }
    val unhandledInputs =
      (unhandledExamples :+ fragmentToInputMap(initialFragmentFromGroup)).map(_._1)
    info("unhandled inputs: " + unhandledInputs)
      
    
    // these predicates will tell us (for these examples, this expression is *not* nil)
    val initialPredicates = calculatePredicatesStructure(unhandledInputs, xs)
    info("initialPredicates: " + initialPredicates.map({ case (k,v) => (k, v.map(x => x._2(Util.w))) } ))
    
    // all variables are Nil
    val fullConditioned = xs

    val initialBranches =
      for ( (examples, predicates) <- initialPredicates) yield {
        val transformedInputs = transformedExamples.map(_._1)
        val inputToFragmentMap = transformedInputs zip unorderedFragmentsAll toMap
        
        val fragments = examples.map(inputToFragmentMap)
        info("fragments: " + fragments)
        
        if (fragments.distinct.size != 1) {
          throw new Exception
        }
        
        val conditionsToRemove = 
          for ((x, predicate) <- predicates) yield predicate(x)
        val currentNils = fullConditioned.toSet -- conditionsToRemove
        val conditions =
          for (varToCheck <- currentNils) yield {
            IsInstanceOf(varToCheck, nilClass)
          }
            
        val branch =
          fragments.head
          
        (and(conditions.toSeq: _*), branch)
      }

    val ifExpr = 
      ((UnitLiteral(): Expr) /: initialBranches.reverse) {
        case (current, (condition, branch)) =>
          IfExpr(condition, branch, current) 
      }
    
    Some((ifExpr, null))
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
  
  /*
   * returns:
   * - list of n fragments (last n one), for which some common form was identified
   * - path to this form ??
   * - mapping for substitution that would equate these fragments
   */
  def calculateFragments(examples: List[IO], xs: List[Variable]) = {
    entering("calculateFragments", examples, xs)
    
    // get fragments
    val unorderedFragmentsAll = Fragmenter.constructFragments(examples, xs)
    
    // XXX -- hack -- make sort do equivalence classes
    val unorderedFragments = unorderedFragmentsAll.filter(_.toString != "Nil")
    info("unordered fragments: " + unorderedFragments.mkString("\n"))
    
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
    assert(compatibles == allPairsCompatibles, "just for merge -- remove me")

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
    assert(filteredDiffGroups.size == 2)
    
    (unorderedFragmentsAll.filter(_.toString == "Nil") ::: emptyDiffs.map(_._1), filteredDiffGroups)
  }
  
  def calculatePredicates(
    filteredDiffGroups: List[(Set[(Expr, Expr)], Iterable[(Map[Variable, Expr], Expr => Expr)])],
    getEnum: TypeTree => Iterable[Expr],
    fragmentsAndInputsMap: Map[Expressions.Expr, InputOutputExample],
    evaluator: Evaluator
  ) = {
//    entering("calculatePredicates", inputExamplesList, xs)
    
    val enum = getEnum(BooleanType)
    
    info("Evaluation")
    val distinguishing =
      for (ex <- enum.take(30);
        _ = info(s"example is $ex");
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
            evaluator.eval(ex, new Model(inputs.toMap)) match {
              case EvaluationResults.Successful(BooleanLiteral(v)) =>
    //            print({if (v.asInstanceOf[BooleanLiteral].value) "t" else "f"})
                info(s"$v for $ex, and inputs ${inputs}")
                Some(v)
              case e: EvaluationResults.EvaluatorError =>
    //            print("_")
                info("evaluation failure: " + e + s" for inputs ${inputs}")
                None
            }
          }
          
        val allEqual = res
        info(s"results for $ex and $set: $allEqual")
        
        assert(allEqual.size > 0)

        if(
          allEqual.filterNot(_.isEmpty).distinct.size == 1
//            allEqual.filterNot(_.isEmpty).distinct.size ==
//            allEqual.filterNot(_.isEmpty).size
        )
          Some((ex, (set, allEqual.filterNot(_.isEmpty).head.get)))
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
    assert(inputExamplesVerbatim.size >= 2)
    val inputExamplesList =
      (inputExamplesVerbatim.head.map(List(_)) /: inputExamplesVerbatim.tail) {
        case (soFarLists, elList) =>
          for ((currVarList, varInput) <- soFarLists zip elList) yield currVarList :+ varInput
      }
    info("unhandledInputsRightForm: " + inputExamplesList)

    // for each variable find predicates
//    val (predicatesList, predicatesFunsList, partitions) = (
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
          
          val partition = (atomExamples zip inputExamples groupBy (_._1) head)._2.map(_._2)
          
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

}
