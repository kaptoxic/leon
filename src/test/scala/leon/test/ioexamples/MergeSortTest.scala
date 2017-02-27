package leon.test
package ioexamples

import leon._
import purescala._
import Expressions._
import Extractors._
import Definitions._
import Types._
import Common._

import leon.solvers._
import leon.solvers.z3._
import leon.solvers.Solver
import leon.synthesis.{ Synthesizer => _, _ }
import leon.synthesis.utils._
import leon.synthesis.ioexamples._
import leon.evaluators._

import leon.utils.logging._

import leon.test.condabd.util._

import org.scalatest._
import org.scalatest.Matchers._

import java.io.{ BufferedWriter, FileWriter, File }

/*
 * NOTES:
 * if we have l1, l2 that makes problem for sorting
 */
class MergeSortTest extends FunSuite with Matchers with Inside with HasLogger {

  import Scaffold._
  import Constructors._
  import UtilCaseClass._

  import ExampleInputs._
  import Differencer.{ differences }
  import Util.w

  val ioExamplesTestcaseDir = "testcases/synthesis/io-examples/"

  //    println(mapOfSubexpressions(f3))
  import scala.language.implicitConversions
  import Util.{ diffsToString, diffToString }

  implicit def expressionsToString(l: Iterable[Expressions.Expr]) =
    l.map({ case a => CustomPrinter(a) }).mkString("\n")

  val problems = forFile(ioExamplesTestcaseDir + "MergeSortMerge.scala").toList
  problems.size should be(2)

  val (sctx, funDef, problem) = problems.head

  implicit val program = sctx.program

  val consClass = program.caseClassDef("Cons").typed
  val nilClass = program.caseClassDef("Nil").typed
  val nilExp = CaseClass(nilClass, Nil): Expr

  val l1 :: l2 :: Nil = problem.as.map(_.toVariable)

  def t(expr: Expr) = {
    caseClassSelector(consClass, expr, consClass.fields.find(_.id.name == "tail").get.id)
    //      CaseClassSelector(consClass, expr, consClass.fields.find(_.id.name == "tail").get.id)
  }

  val l = Variable(FreshIdentifier("l", consClass))

  val extraction = new ExamplesExtraction(sctx, sctx.program)
  val examples = extraction.extract(problem)
  withClue(examples) {
    examples.size should be(5)
  }

  def getEnum = {
    import synthesis.ioexamples.backwards.TermSynthesizer

    val heads =
      caseClassSelector(consClass, l1, consClass.fields.find(_.id.name == "head").get.id) ::
        caseClassSelector(consClass, l2, consClass.fields.find(_.id.name == "head").get.id) :: Nil

    val myGrammar = {
      import leon.grammars._
      import purescala.ExprOps._
      import purescala.Expressions.Expr
      import purescala.Extractors.TopLevelAnds
      import purescala.Types.{ BooleanType, Int32Type, IntegerType }
      import Witnesses.Hint

      val TopLevelAnds(ws) = problem.ws
      val hints = ws.collect { case Hint(e) if formulaSize(e) >= 4 => e }
      val inputs = /*problem.allAs.map(_.toVariable) ++ hints ++*/ heads
      val exclude = sctx.settings.functionsToIgnore
      val recCalls = {
        if (sctx.findOptionOrDefault(SynthesisPhase.optIntroduceRecCalls)) Empty()
        else SafeRecursiveCalls(sctx.program, problem.ws, problem.pc)
      }

      BaseGrammar ||
        Closures ||
        EqualityGrammar(Set(IntegerType, Int32Type, BooleanType) ++ inputs.map { _.getType }) ||
        OneOf(inputs) ||
        Constants(sctx.functionContext.fullBody) ||
        FunctionCalls(sctx.program, sctx.functionContext, inputs.map(_.getType), exclude) ||
        recCalls
    }

    val termSynthesizer = new TermSynthesizer(sctx, problem, inGrammar = Some(myGrammar))

    val enum = termSynthesizer.apply(BooleanType :: Nil)

    enum
  }

  val evaluator = new DefaultEvaluator(sctx, program)

  var filteredGroupsToCompare: Iterable[Option[(Expr, Iterable[(Set[(Expr, Expr)], Boolean)])]] = _
  
  
  test("test synthesizer") {

    val ((inIds, outId), transformedExamples) = ExamplesExtraction.transformMappings(examples).get
    //      info(s"inIds $inIds")
    val unorderedFragments = Fragmenter.constructFragments(transformedExamples, inIds)

    val synthesizer = new Synthesizer

    val Some((body, funDef)) = synthesizer.synthesize(examples.toList, _ => getEnum, evaluator, nilClass)

    info("(body, funDef) is: " + (body, funDef))
    body.toString shouldBe
      """|if ((l1 == Nil) && (l2 == Nil)) {
         |  l2
         |} else if ((l2 == Nil)) {
         |  l1
         |} else if (l1.head <= l2.head) {
         |  Cons(l1.head, rec(l1.tail, l2))
         |} else {
         |  Cons(l2.head, rec(l1, l2.tail))
         |}""".stripMargin

  }


//  test("playing") {
//
//    // get fragments
//    val ((inIds, outId), transformedExamples) = ExamplesExtraction.transformMappings(examples).get
//    info(s"inIds $inIds")
//    val unorderedFragments =
//      Fragmenter.constructFragments(transformedExamples, inIds)
//
//    info("transformed examples: " + transformedExamples.mkString("\n"))
//    info("unordered fragments: " + unorderedFragments.mkString("\n"))
//    info("unordered fragments: " + unorderedFragments.map(CustomPrinter(_)).mkString("\n"))
//
////    [h(l2)l1]
////    [h(l1),h(l2)t(l1)]
////    l2 nil
////    l1
////    [h(l2),h(l1),ht(l2)t(l1)]
//
//    val (f3 :: f4 :: f1 :: f2 :: f5 :: Nil) = unorderedFragments
//    val fragments = f1 :: f2 :: f3 :: f4 :: f5 :: Nil
//    val fragmentNames =
//      (fragments zip (1 to 5).map("f" + _)).toMap
//
//    // kick out fragments that cannot be compared ?
//    val crucialFragments = f2 :: f3 :: f4 :: f5 :: Nil
//    for (crucialFragmentsPerm <- crucialFragments.permutations) {
//      val sortedFragments = Util.sort(crucialFragmentsPerm)
//      withClue(s"${sortedFragments: String} != ${crucialFragments: String}") {
//        sortedFragments shouldBe crucialFragments
//      }
//    }
//
//    info("ordered fragments: " + fragments.mkString("\n"))
//    info("ordered fragments: " + fragments.map(CustomPrinter(_)).mkString("\n"))
//
//    {
//      // comparing l1 and nil should return nothing
//      val diffs = differences(f1, f2, l1 :: l2 :: Nil)
//      diffs should not be('empty)
//    }
//
//    {
//      val diffs = differences(f2, f3, l1 :: l2 :: Nil)
//      diffs.size should be(3)
//
//      val (substMap, toSubstitute) = diffs.head
//
//      diffs.find(_._1 == Map(l1 -> l1)).get._2(l) shouldBe CaseClass(consClass,
//        caseClassField(l2, "head") :: l :: Nil)
//    }
//
//    //    {
//    //      val diffs = differences(f3, f4, l1 :: l2 :: Nil)
//    //      diffs should have size (2)
//    //      
//    //      val (substMap, toSubstitute) = diffs.toSeq(1)
//    //      
//    //      val validDiffs = diffs.filter(!Differencer.areCompatible(_, null).isEmpty)
//    //      diffs.filter should contain (CaseClass(consClass, caseClassField(l2, "head") :: l :: Nil))
//    ////      val substitutedSubExpression = ExprOps.replace(substMap, expr)
//    ////      val newE = ExprOps.replace(substs, expr)
//    //    }
//    //    
//    //    {
//    //      val diffs = differences(f4, f5, l1 :: l2 :: Nil)
//    //      diffs should have size (1)
//    //      
//    //      val (substMap, toSubstitute) = diffs.head
//    //      
//    //      toSubstitute(l) shouldBe t(l)
//    ////      val substitutedSubExpression = ExprOps.replace(substMap, expr)
//    ////      val newE = ExprOps.replace(substs, expr)
//    //    }
//
//    {
//      val allDiffs =
//        for ((f1, f2) <- fragments zip fragments.tail) yield {
//          val diffs = differences(f1, f2, l1 :: l2 :: Nil).map(_._1)
//          diffs.toSet
//        }
//
//      val intersect = allDiffs.reduce(_ intersect _)
//      val union = allDiffs.reduce(_ union _)
//
//      intersect should be('empty)
//      union should not be ('empty)
//      union should contain(Map(l2 -> l2, l1 -> t(l1)))
//      union should contain(Map(l1 -> l1, l2 -> t(l2)))
//
//      allDiffs.tail.count(_ contains Map(l1 -> l1, l2 -> t(l2))) shouldBe 1
//      allDiffs.tail.count(_ contains Map(l2 -> l2, l1 -> t(l1))) shouldBe 1
//    }
//
//    val allDiffs =
//      for ((f1, f2) <- fragments zip fragments.tail) yield {
//        val diffs = differences(f1, f2, l1 :: l2 :: Nil)
//        fine(s"diffs between $f1 and $f2 are ${diffs: String}")
//        (f1, f2, diffs)
//      }
//    info("allDiffs: " + allDiffs.mkString("\n"))
//
//    val compatibles =
//      for (
//        (f11, f21, diffs1) <- allDiffs.tail;
//        (f12, f22, diffs2) <- allDiffs.drop(2);
//        if f11 != f12;
//        diff1 <- diffs1;
//        diff2 <- diffs2;
//        _ = finer(s"Checking diffs: ${diff1: String} and ${diff2: String}");
//        merged <- Differencer.areCompatible(diff1, diff2);
//        _ = finer(s"compatible!!")
//      ) yield (f11 :: f21 :: f12 :: f22 :: Nil map fragmentNames, merged)
//
//    info("compatibles: " + compatibles.map(p => (p._1, (p._2._1, p._2._2(w)))).mkString("\n"))
//
//    compatibles should have size 1
//    inside(compatibles.head) {
//      case (list, (map, fun)) =>
//        list shouldBe List("f2", "f3", "f4", "f5")
//        map shouldBe Map(l1 -> l1, l2 -> t(l2))
//    }
//
//    val enum = getEnum
//
//    info(enum.take(3))
//
//    val evaluator = new DefaultEvaluator(sctx, program)
//
//    info(s"examples: ${examples.reverse.mkString("\n")}")
//
////    for (
////      ex <- enum.take(30);
////      //      if (ex.toString == "l1.head < l2.head");
////      _ = print(s"\n $ex -- ");
////      ind <- List(1, 2, 4, 3, 5);
////      (inputs, output) = examples.reverse(ind - 1);
////      inputMap = inputs.toMap
////    ) {
////      val v1 = inputMap(l1.id)
////      val v2 = inputMap(l2.id)
////      val res = evaluator.eval(ex,
////        new Model(Map(l1.id -> v1, l2.id -> v2)))
////
////      res match {
////        case EvaluationResults.Successful(v) =>
////          print({ if (v.asInstanceOf[BooleanLiteral].value) "t" else "f" })
////        //          info(s"$v for $ex, ${v1}, $v2")
////        case e: EvaluationResults.EvaluatorError =>
////          print("_")
////        //          info("evaluation failure: " + e + s" for $v1 and $v2")
////      }
////
////    }
//
//    val fragmentsAndInputs = fragments zip (List(3, 4, 1, 2, 5) map { in => examples(in - 1) })
//
//    // check if these results match the "compatible groups"
//    {
//      val compositeFragmentsAndInputs =
//        //        fragmentsAndInputs filter {
//        //          case (f, e@((_, Composite(_)) :: (_, Composite(_)) :: Nil, _))  =>
//        //            true
//        //          case _ =>
//        //            false
//        //        }
//        fragmentsAndInputs filter {
//          case (f, e @ ((_, Atom(_)) :: (_, Atom(_)) :: Nil, _)) =>
//            false
//          case _ =>
//            true
//        }
//      val compositeFragmentsAndInputsMap = compositeFragmentsAndInputs.toMap
//      compositeFragmentsAndInputs should have size 4
//      info("compositeFragmentsAndInputs:\n" + compositeFragmentsAndInputs.mkString("\n"))
//
//      val fragments = compositeFragmentsAndInputs.map(_._1)
//
//      val allDiffs =
//        for ((f1, f2) <- fragments zip fragments.tail) yield {
//          val diffs = differences(f1, f2, l1 :: l2 :: Nil)
//          (f1, f2, diffs)
//        }
//      allDiffs should have size 3
//
//      val compatibles =
//        for (
//          (f11, f21, diffs1) <- allDiffs;
//          (f12, f22, diffs2) <- allDiffs.tail;
//          if f11 != f12;
//          diff1 <- diffs1;
//          diff2 <- diffs2;
//          merged <- Differencer.areCompatible(diff1, diff2)
//        ) yield (Set((f11, f21), (f12, f22)), merged)
//
//      // group all merged according to f21 and f22
//      //      type tpe = Map[(Expr, Expr), List[(Map[Variable, Expr], Expr => Expr)]]
//      //      val compatiblesGrouped =
//      //        ((Map(): tpe) /: compatibles.groupBy(_._1)) {
//      //          case (curr, (pair, list)) =>
//      //            for ((pair2, (mapping, fun)) <- list) yield {
//      //              curr
//      //            }
//      //        }
//
//      assert(compatibles.map(_._1).distinct.size == compatibles.size)
//
//      compatibles should have size 1
//
//      // find groups for which you need to find distinguishing predicate
//      val groups =
//        compatibles.map({
//          case (set, map) =>
//            (set, map :: Nil)
//        }) ++
//          allDiffs.filter({
//            case (f1, f2, diff) =>
//              !(compatibles.map(_._1).flatten contains (f1, f2))
//          }).map({ case (f1, f2, diff) => (Set((f1, f2)), diff) })
//
//      groups should have size 2
//
//      info("groups: " + groups.mkString("\n"))
//
//      // FIXME hardcoded, but here we should check decreasing paramters
//      // essentially, remove increasing recursive calls
//      val filteredDiffGroups =
//        groups.map({
//          case (a, b) =>
//            (a,
//              b.filter(_.toString != "(Map(l2 -> l1, l1 -> Cons(l2.head, l1.tail)),<function1>)"))
//        })
//      info(filteredDiffGroups.mkString("\n"))
//      assert(filteredDiffGroups.size == 2)
//
//      info("Evaluation")
//      val distinguishing =
//        for (
//          ex <- enum.take(30);
//          _ = info(s"example is $ex");
//          (set, diffs) <- filteredDiffGroups;
//          _ = assert(diffs.size == 1);
//          (mapping, fun) = diffs.head
//        ) yield {
//          val res =
//            for (
//              (_, f) <- set.toList;
//              // note that the lowest fragment (out of two) might fail
//              //              if (compositeFragmentsAndInputsMap.contains(f));
//              // NOTE we assume evaluation error actually is one of those simple cases that already work 
//              (inputs, _) = compositeFragmentsAndInputsMap(f)
//            ) yield {
//              evaluator.eval(ex, new Model(inputs.toMap)) match {
//                case EvaluationResults.Successful(BooleanLiteral(v)) =>
//                  //            print({if (v.asInstanceOf[BooleanLiteral].value) "t" else "f"})
//                  info(s"$v for $ex, and inputs ${inputs}")
//                  Some(v)
//                case e: EvaluationResults.EvaluatorError =>
//                  //            print("_")
//                  info("evaluation failure: " + e + s" for inputs ${inputs}")
//                  None
//              }
//            }
//
//          val allEqual = res
//          info(s"results for $ex and $set: $allEqual")
//
//          allEqual.size should be > (0)
//
//          if (allEqual.filterNot(_.isEmpty).distinct.size == 1 //            allEqual.filterNot(_.isEmpty).distinct.size ==
//          //            allEqual.filterNot(_.isEmpty).size
//          )
//            Some((ex, (set, allEqual.filterNot(_.isEmpty).head.get)))
//          else
//            None
//        }
//
//      info("distinguishing:\n" + distinguishing.flatten.mkString("\n"))
//      val distinguishedByGroup =
//        distinguishing.flatten.groupBy(_._1).filter({
//          case (k, res) if res.size == 2 =>
//            val results = res.map(_._2._2)
//
//            info(s"for $k we have:\n${results.mkString("\n")}")
//            info(s"${results.toList.distinct}")
//            results.toList.distinct.size == 2
//          case _ =>
//            false
//        }).map({
//          case (k, v) =>
//            (k, v.map({ case (a, (b, c)) => (b, c) }))
//        })
//      info("distinguishing by group:\n" + distinguishedByGroup.mkString("\n"))
//
//      val results =
//        for (
//          (ex, setResults) <- distinguishedByGroup;
//          _ = info(s"example is $ex");
//          (set, diffs) <- filteredDiffGroups;
//          _ = assert(diffs.size == 1);
//          (mapping, fun) = diffs.head;
//          modifiedEx = ExprOps.replaceFromIDs(mapping.map({ case (k, v) => (k.id, v) }), ex)
//        ) yield {
//
//          val res =
//            for (
//              (chainedFragment, f) <- set.toList;
//              // note that the lowest fragment (out of two) might fail
//              //              if (compositeFragmentsAndInputsMap.contains(f));
//              // NOTE we assume evaluation error actually is one of those simple cases that already work 
//              (inputs, _) = compositeFragmentsAndInputsMap(f)
//            ) yield {
//              evaluator.eval(modifiedEx, new Model(inputs.toMap)) match {
//                case EvaluationResults.Successful(BooleanLiteral(v)) =>
//                  //            print({if (v.asInstanceOf[BooleanLiteral].value) "t" else "f"})
//                  info(s"$v for $modifiedEx, and inputs ${inputs}")
//                  val resultForChainedFragment =
//                    setResults.find(_._1.map(_._2) contains chainedFragment)
//                  info(s"resultForChainedFragment: ${resultForChainedFragment}")
//                  if (resultForChainedFragment.isEmpty || v == resultForChainedFragment.get._2) {
//                    true
//                  } else {
//                    false
//                  }
//                case e: EvaluationResults.EvaluatorError =>
//                  //            print("_")
//                  info("evaluation failure: " + e + s" for inputs ${inputs}")
//                  true
//              }
//            }
//
//          val allEqual = res
//          info(s"results for $ex and $set: $allEqual")
//
//          allEqual.size should be > (0)
//
//          if (allEqual.forall(identity))
//            Some((ex, setResults))
//          else
//            None
//        }
//
//      // same as in previous test
//      if (filteredGroupsToCompare == null)
//        filteredGroupsToCompare = results
//      else
//        results shouldBe filteredGroupsToCompare
//
//      info(results.flatten.mkString("\n"))
//
//      // check if the reduction of inputs makes a chains with predicates as well
//      // - optimization -- if you get an existing input by reduction, you know it's fine
//      //    val allOk =
//      //      for ((ex, setResults) <- distinguishedByGroup;
//      //        if (results.flatten.toList contains ex);
//      //        _ = info(s"example is $ex");
//      //        // check all groups
//      //        (fragments, diffs) <- filteredDiffGroups;
//      //        _ = assert(diffs.size == 1);
//      //        (mapping, fun) = diffs.head;
//      //        modifiedEx = ExprOps.replaceFromIDs(mapping.map({ case (k,v) => (k.id, v) }), ex);
//      //      ) {
//      //          
//      //        def getComponentsToCheck(
//      //          currentFragments: (Expr, Expr)
//      //        ): List[Expr] = {
//      //          val (f1, f2) = currentFragments
//      //          if (
//      //        }
//      //          
//      //        
//      //        for((chainedFragment, f) <- set.toList;
//      //      }
//
//    }
//
//  }

	// commented out due to refactoring fragments in synthesizer 
//  // TODO as in the previous testcase, we should make sure the order of fragments and example later on match
//  ignore("synthesis process, with synthesizer") {
//
//    //    println(mapOfSubexpressions(f3))
//    //    import scala.language.implicitConversions
//    //    import Util.{ diffsToString, diffToString }
//    //    
//    //    implicit def expressionsToString(l: Iterable[Expressions.Expr]) =
//    //      l.map({case a => CustomPrinter(a)}).mkString("\n")
//    //    
//    //    val problems = forFile(ioExamplesTestcaseDir + "MergeSortMerge.scala").toList
//    //    problems.size should be (1)
//    //    
//    //    val (sctx, funDef, problem) = problems.head
//    //    
//    //    implicit val program = sctx.program
//    //    
//    //    val consClass = program.caseClassDef("Cons").typed
//    //    val nilClass = program.caseClassDef("Nil").typed
//    //    val nilExp = CaseClass(nilClass, Nil): Expr
//    //    
//    //    def t(expr: Expr) = {
//    //      caseClassSelector(consClass, expr, consClass.fields.find(_.id.name == "tail").get.id)
//    ////      CaseClassSelector(consClass, expr, consClass.fields.find(_.id.name == "tail").get.id)
//    //    }
//    //    
//    //    val l = Variable(FreshIdentifier("l", consClass))
//    //    
//    //    val extraction = new ExamplesExtraction(sctx, sctx.program)
//    //    val examples = extraction.extract(problem)
//    //    withClue(examples) {
//    //      examples.size should be (5)
//    //    }
//
//    // get fragments
//    val ((inIds, outId), transformedExamples) = ExamplesExtraction.transformMappings(examples).get
//    info(s"inIds $inIds")
//    val unorderedFragments = Fragmenter.constructFragments(transformedExamples, inIds)
//
//    val synthesizer = new Synthesizer
//
//    info(s"inIds $inIds")
//    info("transformed examples: " + transformedExamples.mkString("\n"))
//
//    val (emptyDiffs, filteredDiffGroups) = synthesizer.calculateFragments(transformedExamples, inIds.map(_.toVariable))
//    //    assert(emptyDiffs.size == 1)
//    info(filteredDiffGroups.mkString("\n"))
//    assert(filteredDiffGroups.size == 2)
//
//    val enum = getEnum
//
//    val evaluator = new DefaultEvaluator(sctx, program)
//
//    val (f5 :: f3 :: f4 :: f2 :: f1 :: Nil) = unorderedFragments
//    val fragments = f1 :: f2 :: f3 :: f4 :: f5 :: Nil
//    val fragmentsAndInputs: List[(Expressions.Expr, InputOutputExample)] = fragments zip (List(1, 2, 4, 3, 5) map { in => examples.reverse(in - 1) })
//
//    // check if these results match the "compatible groups"
//    val compositeFragmentsAndInputs =
//      //        fragmentsAndInputs filter {
//      //          case (f, e@((_, Composite(_)) :: (_, Composite(_)) :: Nil, _))  =>
//      //            true
//      //          case _ =>
//      //            false
//      //        }
//      fragmentsAndInputs filter {
//        case (f, e @ ((_, Atom(_)) :: (_, Atom(_)) :: Nil, _)) =>
//          false
//        case _ =>
//          true
//      }
//
//    val compositeFragmentsAndInputsMap = compositeFragmentsAndInputs.toMap
//    compositeFragmentsAndInputs should have size 4
//
//    {
//      val fragments = compositeFragmentsAndInputs.map(_._1)
//
//      info("Evaluation")
//      val distinguishing =
//        for (
//          ex <- enum.take(30);
//          _ = info(s"example is $ex");
//          (set, diffs) <- filteredDiffGroups;
//          _ = assert(diffs.size == 1);
//          (mapping, fun) = diffs.head
//        ) yield {
//          val res =
//            for (
//              (_, f) <- set.toList;
//              // note that the lowest fragment (out of two) might fail
//              //              if (compositeFragmentsAndInputsMap.contains(f));
//              // NOTE we assume evaluation error actually is one of those simple cases that already work 
//              (inputs, _) = compositeFragmentsAndInputsMap(f)
//            ) yield {
//              evaluator.eval(ex, new Model(inputs.toMap)) match {
//                case EvaluationResults.Successful(BooleanLiteral(v)) =>
//                  //            print({if (v.asInstanceOf[BooleanLiteral].value) "t" else "f"})
//                  info(s"$v for $ex, and inputs ${inputs}")
//                  Some(v)
//                case e: EvaluationResults.EvaluatorError =>
//                  //            print("_")
//                  info("evaluation failure: " + e + s" for inputs ${inputs}")
//                  None
//              }
//            }
//
//          val allEqual = res
//          info(s"results for $ex and $set: $allEqual")
//
//          allEqual.size should be > (0)
//
//          if (allEqual.filterNot(_.isEmpty).distinct.size == 1 //            allEqual.filterNot(_.isEmpty).distinct.size ==
//          //            allEqual.filterNot(_.isEmpty).size
//          )
//            Some((ex, (set, allEqual.filterNot(_.isEmpty).head.get)))
//          else
//            None
//        }
//
//      info("distinguishing:\n" + distinguishing.flatten.mkString("\n"))
//      val distinguishedByGroup =
//        distinguishing.flatten.groupBy(_._1).filter({
//          case (k, res) if res.size == 2 =>
//            val results = res.map(_._2._2)
//
//            info(s"for $k we have:\n${results.mkString("\n")}")
//            info(s"${results.toList.distinct}")
//            results.toList.distinct.size == 2
//          case _ =>
//            false
//        }).map({
//          case (k, v) =>
//            (k, v.map({ case (a, (b, c)) => (b, c) }))
//        })
//      info("distinguishing by group:\n" + distinguishedByGroup.mkString("\n"))
//
//      val results =
//        for (
//          (ex, setResults) <- distinguishedByGroup;
//          _ = info(s"example is $ex");
//          (set, diffs) <- filteredDiffGroups;
//          _ = assert(diffs.size == 1);
//          (mapping, fun) = diffs.head;
//          modifiedEx = ExprOps.replaceFromIDs(mapping.map({ case (k, v) => (k.id, v) }), ex)
//        ) yield {
//
//          val res =
//            for (
//              (chainedFragment, f) <- set.toList;
//              // note that the lowest fragment (out of two) might fail
//              //              if (compositeFragmentsAndInputsMap.contains(f));
//              // NOTE we assume evaluation error actually is one of those simple cases that already work 
//              (inputs, _) = compositeFragmentsAndInputsMap(f)
//            ) yield {
//              evaluator.eval(modifiedEx, new Model(inputs.toMap)) match {
//                case EvaluationResults.Successful(BooleanLiteral(v)) =>
//                  //            print({if (v.asInstanceOf[BooleanLiteral].value) "t" else "f"})
//                  info(s"$v for $modifiedEx, and inputs ${inputs}")
//                  val resultForChainedFragment =
//                    setResults.find(_._1.map(_._2) contains chainedFragment)
//                  info(s"resultForChainedFragment: ${resultForChainedFragment}")
//                  if (resultForChainedFragment.isEmpty || v == resultForChainedFragment.get._2) {
//                    true
//                  } else {
//                    false
//                  }
//                case e: EvaluationResults.EvaluatorError =>
//                  //            print("_")
//                  info("evaluation failure: " + e + s" for inputs ${inputs}")
//                  true
//              }
//            }
//
//          val allEqual = res
//          info(s"results for $ex and $set: $allEqual")
//
//          allEqual.size should be > (0)
//
//          if (allEqual.forall(identity))
//            Some((ex, setResults))
//          else
//            None
//        }
//
//      // same as in previous test
//      if (filteredGroupsToCompare == null)
//        filteredGroupsToCompare = results
//      else
//        results shouldBe filteredGroupsToCompare
//
//      info(results.flatten.mkString("\n"))
//
//    }
//
//  }
//
//  ignore("calculate predicates structure asdasdasd") {
//
//    val ((inIds, outId), transformedExamples) = ExamplesExtraction.transformMappings(examples).get
//    //      info(s"inIds $inIds")
//    val unorderedFragments = Fragmenter.constructFragments(transformedExamples, inIds)
//
//    val synthesizer = new Synthesizer
//
//    info(s"inIds $inIds")
//    info("transformed examples: " + transformedExamples.mkString("\n"))
//
//    val (emptyDiffs, filteredDiffGroups) = synthesizer.calculateFragments(transformedExamples, inIds.map(_.toVariable))
//    //    assert(emptyDiffs.size == 1)
//    info(filteredDiffGroups.mkString("\n"))
//    //    assert(emptyDiffs.size == 1)
//
//    val (f5 :: f3 :: f4 :: f2 :: f1 :: Nil) = unorderedFragments
//    val fragments = f1 :: f2 :: f3 :: f4 :: f5 :: Nil
//    val fragmentsAndInputs: List[(Expressions.Expr, InputOutputExample)] = fragments zip (List(1, 2, 4, 3, 5) map { in => examples.reverse(in - 1) })
//    val fragmentsAndInputsMap = fragmentsAndInputs.toMap
//
//    // empty diffs is empty
//    val inputsUnmaapped =
//      //(f1 :: f2 :: emptyDiffs.map(_._1) map { x => fragmentsAndInputsMap(x) } map { _._1.map(_._2) }
//      (examples.reverse(0)._1.head._2 :: examples.reverse(1)._1.head._2 :: examples.reverse(3)._1.head._2 :: Nil) ::
//        (examples.reverse(0)._1.tail.head._2 :: examples.reverse(1)._1.tail.head._2 :: examples.reverse(3)._1.tail.head._2 :: Nil) ::
//        Nil
//
//    val inputs = inputsUnmaapped.map(_.map(Util.substituteAllAtom))
//
//    info("inputs are: " + inputs.reverse)
//
//    val predicates = Predicates.calculatePredicates(inputs.reverse, inIds map (_.toVariable))
//
//    println(predicates.map(_.map(_(w))))
//
//  }
//
//  ignore("synthesis process, with synthesizer and evaluator") {
//
//    // get fragments
//    val ((inIds, outId), transformedExamples) = ExamplesExtraction.transformMappings(examples).get
//    info(s"inIds $inIds")
//    val unorderedFragments = Fragmenter.constructFragments(transformedExamples, inIds)
//
//    val synthesizer = new Synthesizer
//
//    info(s"inIds $inIds")
//    info("transformed examples: " + transformedExamples.mkString("\n"))
//
//    val (emptyDiffs, filteredDiffGroups) = synthesizer.calculateFragments(transformedExamples, inIds.map(_.toVariable))
//    //    assert(emptyDiffs.size == 1)
//    info(filteredDiffGroups.mkString("\n"))
//    assert(filteredDiffGroups.size == 2)
//
//    val enum = getEnum
//
//    val evaluator = new DefaultEvaluator(sctx, program)
//
//    val (f5 :: f3 :: f4 :: f2 :: f1 :: Nil) = unorderedFragments
//    val fragments = f1 :: f2 :: f3 :: f4 :: f5 :: Nil
//    val fragmentsAndInputs: List[(Expressions.Expr, InputOutputExample)] = fragments zip (List(1, 2, 4, 3, 5) map { in => examples.reverse(in - 1) })
//
//    // check if these results match the "compatible groups"
//    val compositeFragmentsAndInputs =
//      //        fragmentsAndInputs filter {
//      //          case (f, e@((_, Composite(_)) :: (_, Composite(_)) :: Nil, _))  =>
//      //            true
//      //          case _ =>
//      //            false
//      //        }
//      fragmentsAndInputs filter {
//        case (f, e @ ((_, Atom(_)) :: (_, Atom(_)) :: Nil, _)) =>
//          false
//        case _ =>
//          true
//      }
//
//    val compositeFragmentsAndInputsMap = compositeFragmentsAndInputs.toMap
//    compositeFragmentsAndInputs should have size 4
//
//    val results =
//      synthesizer.calculatePredicates(
//        filteredDiffGroups,
//        _ => getEnum,
//        compositeFragmentsAndInputsMap,
//        evaluator)
//
//    // same as in previous test
//    if (filteredGroupsToCompare == null)
//      filteredGroupsToCompare = results
//    else
//      results shouldBe filteredGroupsToCompare
//
//
//    info(results.flatten.mkString("\n"))
//
//  }
//  
//  ignore("evaluator finding the needed expressions") {
//    
//    val (sctx, fundDef, problem) = problems.find(_._2.id.name == "sort").get
//    
//    val listType = program.definedClasses.find(_.id.name == "List").get.typed
//    val pairType = program.definedClasses.find(_.id.name == "Pair").get.typed
//    
//    def getEnum(tpe: TypeTree) = {
//      import synthesis.ioexamples.backwards.TermSynthesizer
//
//      import leon.grammars._
//    
//      case class CaseClassSelectors(prog: Program, currentFunction: FunDef, types: Seq[TypeTree]) extends SimpleExpressionGrammar {
//        def computeProductions(t: TypeTree)(implicit ctx: LeonContext): Seq[Prod] = {
//      
//          for (
//            cd <- program.definedClasses;
//            if cd.isInstanceOf[CaseClassDef];
//            cdd = cd.asInstanceOf[CaseClassDef];
//            field <- cdd.fields
//          ) yield {
//            nonTerminal(cd.typed :: Nil,
//              x => caseClassSelector(cdd.typed, x.head, field.id)
//            )
//          }
//        }
//      }
//  
//      def fields: List[CaseClassSelector] = Nil
//      //      caseClassSelector(consClass, l1, consClass.fields.find(_.id.name == "head").get.id) ::
//      //        caseClassSelector(consClass, l2, consClass.fields.find(_.id.name == "head").get.id) :: Nil
//  
//      val myGrammar = {
//        import leon.grammars._
//        import purescala.ExprOps._
//        import purescala.Expressions.Expr
//        import purescala.Extractors.TopLevelAnds
//        import purescala.Types.{ BooleanType, Int32Type, IntegerType }
//        import Witnesses.Hint
//  
//        val TopLevelAnds(ws) = problem.ws
//        val hints = ws.collect { case Hint(e) if formulaSize(e) >= 4 => e }
//        val inputs = problem.allAs.map(_.toVariable) ++ hints ++ fields
//        val exclude = sctx.settings.functionsToIgnore
//        val recCalls = {
//          if (sctx.findOptionOrDefault(SynthesisPhase.optIntroduceRecCalls)) Empty()
//          else SafeRecursiveCalls(sctx.program, problem.ws, problem.pc)
//        }
//  
//        BaseGrammar ||
//          Closures ||
//          EqualityGrammar(Set(IntegerType, Int32Type, BooleanType) ++ inputs.map { _.getType }) ||
//          OneOf(inputs) ||
//          Constants(sctx.functionContext.fullBody) ||
//          FunctionCalls(sctx.program, sctx.functionContext, inputs.map(_.getType), exclude) ||
//          CaseClassSelectors(sctx.program, sctx.functionContext, inputs.map(_.getType)) ||
//          recCalls
//      }
//  
//      val termSynthesizer = new TermSynthesizer(sctx, problem, inGrammar = Some(myGrammar), sizes = (1, 5))
//  
//      val enum = termSynthesizer.apply(tpe :: Nil)
//  
//      enum
//    }
//    
//    val enum = getEnum(pairType)
//
//    for (
//      ex <- enum.take(30)
//    )
//      println(ex)
//
//  }
}