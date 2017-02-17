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
import leon.synthesis._
import leon.synthesis.utils._
import leon.synthesis.ioexamples._
import leon.evaluators._

import leon.utils.logging._

import leon.test.condabd.util._

import org.scalatest._
import org.scalatest.Matchers._

import java.io.{ BufferedWriter, FileWriter, File }

class MergeSortTest extends FunSuite with Matchers with Inside with HasLogger {

  import Scaffold._
  import Constructors._
  import UtilCaseClass._

  import ExampleInputs._
  import Differencer.{ differences }
  import Util.w

  val ioExamplesTestcaseDir = "testcases/synthesis/io-examples/"


  test("playing") {
    
//    println(mapOfSubexpressions(f3))
    import scala.language.implicitConversions
    implicit def diffToString(l: (Map[Expressions.Variable, Expressions.Expr],
      Expressions.Expr => Expressions.Expr) ) = (l._1, l._2(w)).toString
    implicit def diffsToString(l: Iterable[(Map[Expressions.Variable, Expressions.Expr],
      Expressions.Expr => Expressions.Expr)]) = l.map(diffToString).mkString("\n")
    def println(s: String) = scala.Predef.println(s)
    implicit def expressionsToString(l: Iterable[Expressions.Expr]) =
      l.map({case a => CustomPrinter(a)}).mkString("\n")
    
    val problems = forFile(ioExamplesTestcaseDir + "MergeSortMerge.scala").toList
    problems.size should be (1)
    
    val (sctx, funDef, problem) = problems.head
    
    implicit val program = sctx.program
    
    val consClass = program.caseClassDef("Cons").typed
    val nilClass = program.caseClassDef("Nil").typed
    val nilExp = CaseClass(nilClass, Nil): Expr
    
    def t(expr: Expr) = {
      caseClassSelector(consClass, expr, consClass.fields.find(_.id.name == "tail").get.id)
//      CaseClassSelector(consClass, expr, consClass.fields.find(_.id.name == "tail").get.id)
    }
    
    val l = Variable(FreshIdentifier("l", consClass))
    
    val extraction = new ExamplesExtraction(sctx, sctx.program)
    val examples = extraction.extract(problem)
    withClue(examples) {
      examples.size should be (5)
    }
    
    
    // get fragments
    val ((inIds, outId), transformedExamples) = extraction.transformMappings(examples).get
    info(s"inIds $inIds")
    val unorderedFragments = Fragmenter.constructFragments(transformedExamples, inIds)
    
    info("transformed examples: " +transformedExamples.mkString("\n"))
    info("unordered fragments: " + unorderedFragments.mkString("\n"))
    info("unordered fragments: " + unorderedFragments.map(CustomPrinter(_)).mkString("\n"))
    
    val l1 :: l2 :: Nil = problem.as.map(_.toVariable)
    
//    [h(l1),h(l2)t(l1)]
//    [h(l2)l1]
//    l1
//    [h(l2),h(l1),ht(l2)t(l1)]
//    nil
    val (f4 :: f3 :: f2 :: f5 :: f1 :: Nil) = unorderedFragments
    val fragments = f1 :: f2 :: f3 :: f4 :: f5 :: Nil
    val fragmentNames =
      (fragments zip (1 to 5).map("f" + _)).toMap
      
    // kick out fragments that cannot be compared ?
    val crucialFragments = f2 :: f3 :: f4 :: f5 :: Nil
    for (crucialFragmentsPerm <- crucialFragments.permutations) {
      val sortedFragments = Util.sort(crucialFragmentsPerm)
      withClue(s"${sortedFragments: String} != ${crucialFragments: String}") {
        sortedFragments shouldBe crucialFragments
      }
    }
    
    info("ordered fragments: " + fragments.mkString("\n"))
    info("ordered fragments: " + fragments.map(CustomPrinter(_)).mkString("\n"))
    
    {
      // comparing l1 and nil should return nothing
      val diffs = differences(f1, f2, l1 :: l2 :: Nil)
      diffs should be ('empty)
    }
    
    {
      val diffs = differences(f2, f3, l1 :: l2 :: Nil)
      diffs.size should be (3)
      
      val (substMap, toSubstitute) = diffs.head
      
      diffs.find(_._1 == Map(l1 -> l1)).get._2(l) shouldBe CaseClass(consClass,
        caseClassField(l2, "head") :: l :: Nil)
    }
    
//    {
//      val diffs = differences(f3, f4, l1 :: l2 :: Nil)
//      diffs should have size (2)
//      
//      val (substMap, toSubstitute) = diffs.toSeq(1)
//      
//      val validDiffs = diffs.filter(!Differencer.areCompatible(_, null).isEmpty)
//      diffs.filter should contain (CaseClass(consClass, caseClassField(l2, "head") :: l :: Nil))
////      val substitutedSubExpression = ExprOps.replace(substMap, expr)
////      val newE = ExprOps.replace(substs, expr)
//    }
//    
//    {
//      val diffs = differences(f4, f5, l1 :: l2 :: Nil)
//      diffs should have size (1)
//      
//      val (substMap, toSubstitute) = diffs.head
//      
//      toSubstitute(l) shouldBe t(l)
////      val substitutedSubExpression = ExprOps.replace(substMap, expr)
////      val newE = ExprOps.replace(substs, expr)
//    }
    
    {
      val allDiffs =
  	    for((f1, f2) <- fragments zip fragments.tail) yield {
  	      val diffs = differences(f1, f2, l1 :: l2 :: Nil).map(_._1)
  		    diffs.toSet
  	    }
      
      val intersect = allDiffs.reduce(_ intersect _)
      val union = allDiffs.reduce(_ union _)
  
      intersect should be ('empty)
      union should not be ('empty)
      union should contain (Map(l2 -> l2, l1 -> t(l1)))
      union should contain (Map(l1 -> l1, l2 -> t(l2)))
      
      allDiffs.tail.count(_ contains Map(l1 -> l1, l2 -> t(l2))) shouldBe 1
      allDiffs.tail.count(_ contains Map(l2 -> l2, l1 -> t(l1))) shouldBe 1
    }
    
    val allDiffs =
	    for((f1, f2) <- fragments zip fragments.tail) yield {
	      val diffs = differences(f1, f2, l1 :: l2 :: Nil)
	      fine(s"diffs between $f1 and $f2 are ${diffs: String}")
	      (f1, f2, diffs)
	    }
    
    val compatibles =
      for ((f11, f21, diffs1) <- allDiffs.tail;
        (f12, f22, diffs2) <- allDiffs.drop(2);
        if f11 != f12;
        diff1 <- diffs1;
        diff2 <- diffs2;
        _ = finer(s"Checking diffs: ${diff1: String} and ${diff2: String}");
        merged <- Differencer.areCompatible(diff1, diff2);
        _ = finer(s"compatible!!")
      ) yield (f11 :: f21 :: f12 :: f22 :: Nil map fragmentNames, merged)
        
    info("compatibles: " + compatibles.map(p => (p._1, (p._2._1, p._2._2(w)))).mkString("\n"))

    compatibles should have size 1
    inside(compatibles.head) {
      case (list, (map, fun)) =>
        list shouldBe List("f2", "f3", "f4", "f5")
        map shouldBe Map(l1 -> l1, l2 -> t(l2))
    }
    
    import synthesis.ioexamples.backwards.TermSynthesizer
    
    val heads =
      caseClassSelector(consClass, l1, consClass.fields.find(_.id.name == "head").get.id) ::
      caseClassSelector(consClass, l2, consClass.fields.find(_.id.name == "head").get.id) :: Nil
      
    val myGrammar = {
      import leon.grammars._
      import purescala.ExprOps._
      import purescala.Expressions.Expr
      import purescala.Extractors.TopLevelAnds
      import purescala.Types.{BooleanType, Int32Type, IntegerType}
      import Witnesses.Hint
      
      val TopLevelAnds(ws) = problem.ws
      val hints = ws.collect{ case Hint(e) if formulaSize(e) >= 4 => e }
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
    
    info(enum.take(3))
    
    val evaluator = new DefaultEvaluator(sctx, program)
    
    info(s"examples: ${examples.reverse.mkString("\n")}")
    
    for (ex <- enum.take(30);
//      if (ex.toString == "l1.head < l2.head");
      _ = print(s"\n $ex -- ");
      ind <- List(1, 2, 4, 3, 5);
      (inputs, output) = examples.reverse(ind - 1);
      inputMap = inputs.toMap
    ) {
      val v1 = inputMap(l1.id)
      val v2 = inputMap(l2.id)
      val res = evaluator.eval(ex,
        new Model(Map(l1.id -> v1, l2.id -> v2)))
      
      res match {
        case EvaluationResults.Successful(v) =>
          print({if (v.asInstanceOf[BooleanLiteral].value) "t" else "f"})
//          info(s"$v for $ex, ${v1}, $v2")
        case e: EvaluationResults.EvaluatorError =>
          print("_")
//          info("evaluation failure: " + e + s" for $v1 and $v2")
      }
      
    }
    
    val fragmentsAndInputs = fragments zip (List(1, 2, 4, 3, 5) map { in => examples.reverse(in - 1)})
    
    // check if these results match the "compatible groups"
    {
      val compositeFragmentsAndInputs =
//        fragmentsAndInputs filter {
//          case (f, e@((_, Composite(_)) :: (_, Composite(_)) :: Nil, _))  =>
//            true
//          case _ =>
//            false
//        }
        fragmentsAndInputs filter {
          case (f, e@((_, Atom(_)) :: (_, Atom(_)) :: Nil, _))  =>
            false
          case _ =>
            true
        }
      val compositeFragmentsAndInputsMap = compositeFragmentsAndInputs.toMap
      compositeFragmentsAndInputs should have size 4
      
      val fragments = compositeFragmentsAndInputs.map(_._1)
      
      val allDiffs =
  	    for((f1, f2) <- fragments zip fragments.tail) yield {
  	      val diffs = differences(f1, f2, l1 :: l2 :: Nil)
  	      (f1, f2, diffs)
  	    }
      allDiffs should have size 3
      
      val compatibles =
        for ((f11, f21, diffs1) <- allDiffs;
          (f12, f22, diffs2) <- allDiffs.tail;
          if f11 != f12;
          diff1 <- diffs1;
          diff2 <- diffs2;
          merged <- Differencer.areCompatible(diff1, diff2)
        ) yield (Set((f11, f21), (f12, f22)), merged)
        
      // group all merged according to f21 and f22
//      type tpe = Map[(Expr, Expr), List[(Map[Variable, Expr], Expr => Expr)]]
//      val compatiblesGrouped =
//        ((Map(): tpe) /: compatibles.groupBy(_._1)) {
//          case (curr, (pair, list)) =>
//            for ((pair2, (mapping, fun)) <- list) yield {
//              curr
//            }
//        }
        
      assert(compatibles.map(_._1).distinct.size == compatibles.size)
          
      compatibles should have size 1
      
      // find groups for which you need to find distinguishing predicate
      val groups =
      compatibles.map({
        case (set, map) =>
          (set, map :: Nil)
      }) ++
      allDiffs.filter({
        case (f1, f2, diff) =>
          ! (compatibles.map(_._1).flatten contains (f1, f2))
      }).map({ case (f1, f2, diff) => (Set((f1, f2)), diff) })

      groups should have size 2
      
      println("")
      
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
              (inputs, _) = compositeFragmentsAndInputsMap(f)) yield {
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
          
          allEqual.size should be > (0)

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
              (inputs, _) = compositeFragmentsAndInputsMap(f)) yield {
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
          
          allEqual.size should be > (0)

          if(allEqual.forall(identity))
            Some(ex)
          else
            None
        }
     
      info(results.flatten.mkString("\n"))
    
    // check if the reduction of inputs makes a chains with predicates as well
    // - optimization -- if you get an existing input by reduction, you know it's fine
//    val allOk =
//      for ((ex, setResults) <- distinguishedByGroup;
//        if (results.flatten.toList contains ex);
//        _ = info(s"example is $ex");
//        // check all groups
//        (fragments, diffs) <- filteredDiffGroups;
//        _ = assert(diffs.size == 1);
//        (mapping, fun) = diffs.head;
//        modifiedEx = ExprOps.replaceFromIDs(mapping.map({ case (k,v) => (k.id, v) }), ex);
//      ) {
//          
//        def getComponentsToCheck(
//          currentFragments: (Expr, Expr)
//        ): List[Expr] = {
//          val (f1, f2) = currentFragments
//          if (
//        }
//          
//        
//        for((chainedFragment, f) <- set.toList;
//      }


      
    }
    
  }

}
