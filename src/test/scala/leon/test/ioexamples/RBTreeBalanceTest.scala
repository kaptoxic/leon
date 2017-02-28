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

class RBTreeBalanceTest extends FunSuite with Matchers with Inside with HasLogger {

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

  val problems = forFile(ioExamplesTestcaseDir + "RedBlackTreeBalance.scala").toList

  test("check synthesis problem") {
    problems.size should be(2)
  }

  val (sctx, funDef, problem) = problems.find(_._2.id.name == "balanceTreeInput").get

  implicit val program = sctx.program

  def getEnum(tpe: TypeTree) = {
    import synthesis.ioexamples.backwards.TermSynthesizer

    def fields: List[CaseClassSelector] = Nil
    //      caseClassSelector(consClass, l1, consClass.fields.find(_.id.name == "head").get.id) ::
    //        caseClassSelector(consClass, l2, consClass.fields.find(_.id.name == "head").get.id) :: Nil

    val myGrammar = {
      import leon.grammars._
      import purescala.ExprOps._
      import purescala.Expressions.Expr
      import purescala.Extractors.TopLevelAnds
      import purescala.Types.{ BooleanType, Int32Type, IntegerType }
      import Witnesses.Hint

      val TopLevelAnds(ws) = problem.ws
      val hints = ws.collect { case Hint(e) if formulaSize(e) >= 4 => e }
      val inputs = /*problem.allAs.map(_.toVariable) ++ hints ++*/ fields
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
        //        FunctionCalls(sctx.program, sctx.functionContext, inputs.map(_.getType), exclude) ||
        recCalls
    }

    val termSynthesizer = new TermSynthesizer(sctx, problem, inGrammar = Some(myGrammar), sizes = (10, 16))

    val enum = termSynthesizer.apply(tpe :: Nil)

    enum
  }

  //  val evaluator = new DefaultEvaluator(sctx, program)
  val evaluator = new CodeGenEvaluator(sctx, program)

  import scife._
  import enumeration.{Map => _, _}
  import dependent._
  import memoization._
  import scife.{ enumeration => e }
  import scife.util._
  
  val leaf =
    CaseClass(program.caseClassDef("Empty").typed, Nil)
  val nodeClass = program.caseClassDef("Node").typed
  val red = CaseClass(program.caseClassDef("Red").typed, Nil)
  val black = CaseClass(program.caseClassDef("Black").typed, Nil)

  def constructEnumerator_new(implicit ms: MemoizationScope) = {
    import enumeration.dependent._

    val treesOfSize: Depend[(Int, Range, Range, Int), Expr] = Depend.memoized(
      (self: Depend[(Int, Range, Range, Int), Expr], pair: (Int, Range, Range, Int)) => {
        val (size, range, colors, blackHeight) = pair

        if (range.size >= size && range.size < 0 || blackHeight < 0) e.Empty
        else if (size == 0 && blackHeight == 1 && colors.end >= 1) e.Singleton(leaf)
        else if (size > 0 && blackHeight >= 1) {
          val roots: Finite[Int] = e.Enum(range)
          val leftSizes: Finite[Int] = e.WrapArray(0 until size)
          val rootColors: Finite[Int] = e.WrapArray(colors.toArray)

          val rootLeftSizePairs = e.Product(leftSizes, roots)
          val rootLeftSizeColorTuples: Finite[((Int, Int), Int)] = e.Product(rootLeftSizePairs, rootColors)

          val leftTrees: Depend[((Int, Int), Int), Expr] = InMap(self, { (par: ((Int, Int), Int)) =>
            val ((leftSize, median), rootColor) = par
            val childColors = if (rootColor == 1) 0 to 1 else 1 to 1
            val childBlackHeight = if (rootColor == 1) blackHeight - 1 else blackHeight
            (leftSize, range.start to (median - 1), childColors, childBlackHeight)
          })

          val rightTrees: Depend[((Int, Int), Int), Expr] = InMap(self, { (par: ((Int, Int), Int)) =>
            val ((leftSize, median), rootColor) = par
            val childColors = if (rootColor == 1) 0 to 1 else 1 to 1
            val childBlackHeight = if (rootColor == 1) blackHeight - 1 else blackHeight
            (size - leftSize - 1, (median + 1) to range.end, childColors, childBlackHeight)
          })

          val leftRightPairs: Depend[((Int, Int), Int), (Expr, Expr)] =
            Product(leftTrees, rightTrees)

          val allNodes =
            memoization.Chain[((Int, Int), Int), (Expr, Expr), Expr](rootLeftSizeColorTuples, leftRightPairs,
              (p1: ((Int, Int), Int), p2: (Expr, Expr)) => {
                val (((leftSize, currRoot), rootColor), (leftTree, rightTree)) = (p1, p2)

                val color = if (rootColor == 1) black else red
                CaseClass(nodeClass, color :: leftTree :: IntLiteral(currRoot) :: rightTree :: Nil)
              })

          allNodes
        } else e.Empty
      })

    treesOfSize
  }
//
// ignore("datastructure generation") {
//   
//
//   val ms = new scope.AccumulatingScope
//   val enum = constructEnumerator_new(ms)
//
//   val elements = (
//     for (size <- 3 to 5) yield {
//       for (
//         blackHeight <- 1 to (Math.log2(size + 1).toInt + 1);
//         e = enum.getEnum(size, 1 to size, 0 to 1, blackHeight);
//         ind <- 0 until e.size
//       ) yield e(ind)
//     }).flatten
//
//   elements should have size 25
//
// }

  ignore("predicates") {
    val eb = problem.eb

    info("invalids:\n" + eb.invalids.mkString("\n"))
    info("valids:\n" + eb.valids.mkString("\n"))

    problem.hasOutputTests shouldBe false

    problem.xs should have size 1

    val resType = problem.xs.head.getType

    val ms = new scope.AccumulatingScope
    val enum = constructEnumerator_new(ms)

    info("going into enumeration")

    val firstNNormal = (
      for (size <- 4 to 8) yield {
        for (
          blackHeight <- 1 to (Math.log2(size + 1).toInt + 1);
          e = enum.getEnum(size, 1 to size, 0 to 1, blackHeight);
          ind <- 0 until e.size
        ) yield e(ind)
      }).flatten
      
    info("enumerated datastructs: " + firstNNormal.mkString("\n"))
        
    val firstNReverted =
      for (
        tree <- firstNNormal
      ) yield {
        tree match {
          case
            CaseClass(`nodeClass`,
              `black` ::
              CaseClass(`nodeClass`,
                `red` :: ll :: lv :: lr :: Nil) ::
              v ::
              CaseClass(`nodeClass`,
                `red` :: rl :: rv :: rr :: Nil) ::
            Nil) =>
              val v1 =
                CaseClass(nodeClass,
                  black ::
                  CaseClass(nodeClass,
                    red ::
                    CaseClass(nodeClass,
                      red :: ll :: lv :: lr :: Nil) ::
                    v :: rl :: Nil) ::
                  rv ::
                  rr ::
                Nil)
                
              val v2 =
                CaseClass(nodeClass,
                  black ::
                  CaseClass(nodeClass,
                    red :: ll :: lv ::
                    CaseClass(nodeClass,
                      red :: lr :: v :: rl :: Nil) :: Nil) ::
                  rv ::
                  rr ::
                Nil)
                
              val v3 =
                CaseClass(nodeClass,
                  black ::
                  ll ::
                  lv ::
                  CaseClass(nodeClass,
                    red ::
                    CaseClass(nodeClass,
                      red :: lr :: v :: rl :: Nil) ::
                    rv :: rr :: Nil) ::
                Nil)
                
              val v4 =
                CaseClass(nodeClass,
                  black ::
                  ll ::
                  lv ::
                  CaseClass(nodeClass,
                    red :: lr :: v ::
                    CaseClass(nodeClass,
                      red :: rl :: rv :: rr :: Nil) :: Nil) ::
                Nil)
                
              v1 :: v2 :: v3 :: v4 :: Nil
          case _ =>
            Nil
        }
      }

    val firstN =
      firstNNormal ++
      firstNReverted.flatten

    val numOfExamples = 42
      
    info("firstN:\n" + firstN.zipWithIndex.mkString("\n"))
//    firstN.size shouldBe numOfExamples

    val filteredExamples =
    {
      val phi = problem.phi

      problem.as should have size 1
      val in = problem.as.head
      problem.xs should have size 1
      val out = problem.xs.head
      val pc = problem.pc
      val toEvaluate = pc.toClause
      val compiled = evaluator.compile(toEvaluate, in :: Nil).get

      for (ex1 <- firstN) yield {
        val res = compiled(new Model(Map(in -> ex1)))

        res match {
          case EvaluationResults.Successful(BooleanLiteral(v)) if v =>
            info("pass precondition for: " + ex1)
            Some(ex1)
          case e: EvaluationResults.EvaluatorError =>
            None
          case _                                   =>
            None
        }
      }

    }

    //    "output finding" -
    {
      val phi = problem.phi

      problem.as should have size 1
      val in = problem.as.head

      problem.xs should have size 1
      val out = problem.xs.head

//      val results = collection.mutable.Map[Expr, Expr]()//.withDefaultValue(Set())     
//      val results = collection.mutable.Map[Expr, Set[Expr]]().withDefaultValue(Set())     
      val results = new collection.mutable.MutableList[(Expr, Expr)]()

//      import scala.util.Random
//      val randomGen = new Random("random number my version".hashCode)
//      val randoms = Seq.fill(300)(randomGen.nextInt(numOfExamples))
//      info("randoms " + randoms)

      val pc = problem.pc
      val toEvaluate = And(pc.toClause, phi)
      val compiled = evaluator.compile(toEvaluate, in :: out :: Nil).get
            
      filteredExamples.flatten.size shouldBe 36
//      info(filteredExamples.map(p => "in : " + p).mkString("\n"))

//      for (ex1 <- firstN) {
      for (ex1 <- filteredExamples.flatten) {
        //        _ = info("*******");
        var flag = true
        val ex2it = firstN.iterator;
        while (flag && ex2it.nonEmpty) {
          val ex2 = ex2it.next
          //        info("toEvaluate " + toEvaluate)

          //        info(s"for in $ex1, out $ex2")
          //          val res = evaluator.eval(toEvaluate, new Model(Map(in -> ex1, out -> ex2)))
          val res = compiled(new Model(Map(in -> ex1, out -> ex2)))
          //          info(s"for in $ex1, out $ex2, got $res")

          res match {
            case EvaluationResults.Successful(BooleanLiteral(v)) if v =>
              //            info(s"for in $ex1, out $ex2")
              //            info("***")
              //              info(s"for input $ex1\n, output $ex2\n existing result is ${results.getOrElse(ex1, w)}\n") 
              //              withClue(s"for input $ex1\n, output $ex2\n existing result is ${results.getOrElse(ex1, w)}\n") {
              //                results.getOrElse(ex1, ex2) shouldBe ex2
              //              results should not contain key (ex1)
              //              }

//              assert(!(results contains ex1))
//              assert(results.getOrElse(ex1, ex2))
//              results(ex1) += ex2
              results += ((ex1, ex2))
            //          info(s"$v for $ex, ${v1}, $v2")
            case e: EvaluationResults.EvaluatorError =>
            //          info("evaluation failure: " + e + s" for $v1 and $v2")
            case _                                   =>
          }
        }
      }

      results.size shouldBe >(0)
//      info(results.map(p => "in : " + p._1 + "\nout: " + p._2).mkString("\n"))
//      results should have size (56)

//      info(s"result with more than 6 results: " + {
//        val (k, v) = results.groupBy(_._1).find(_._2.size > 1).get
//        val v2 = v.map({ case (k, v) => v })
//        k + "\n" + v2
//      })

      val extraction = new ExamplesExtraction(sctx, sctx.program)
      
//      val resultsSmallest = results map {
//        case (k, v) =>
//          (k, v.toList.sortBy(ExprOps.formulaSize _).head)
//      }

      val examples = results.map({
        case (inEx, outEx) =>
          ((in, inEx) :: Nil, (out, outEx))
      }).toList

      //      info("examples\n" + examples.mkString("\n"))
//      examples should have size (56)

      //      test: attempt synthesis
      // get fragments
      val ((inIds, outId), transformedExamples) = ExamplesExtraction.transformMappings(examples).get
      info(s"inIds $inIds")
      info("transformed examples: " + transformedExamples.mkString("\n"))
      val unorderedFragments = Fragmenter.constructFragments(transformedExamples, inIds)
      info("unordered fragments: " + unorderedFragments.mkString("\n"))
      
      val unorederdFragmentsSet = unorderedFragments.toSet
      info("unorederdFragmentsSet:\n" + unorederdFragmentsSet.mkString("\n"))
      
      transformedExamples should have size unorderedFragments.size
      val zipped = (transformedExamples zip unorderedFragments)
      
      val zippedSorted = zipped.sortBy(p => ExprOps.formulaSize(p._2))
      
      info("" + ioexamples.Util.mapOfSubexpressionsToPathFunctions(zippedSorted.last._1._1.head).map(
        {case (k,v) => "" + k + "\n" + v(w) }))
      info("" + zippedSorted.last)
      
      val taken = new collection.mutable.ListBuffer[((List[Expressions.Expr], Expressions.Expr), Expressions.Expr)]()
      var covering = transformedExamples.map(_._1).toSet

      val coveringPairs =
        zippedSorted.takeWhile({
          case _ if covering.isEmpty =>
            false
          case p@(exPair, fragment) =>
            covering = covering - exPair._1
            taken += p
            true
        })
        
      info(s"zipped size ${zipped.size}; coveringPairs size: " + coveringPairs.size)
      info("covering pairs\n: " + coveringPairs.mkString("\n"))
      
      val groupped = zipped.groupBy(_._1._1)
      val sorted =
        for ( (input, list) <- groupped ) yield {
          val fragments = list.map(_._2)
          (input, fragments.sortBy(ExprOps.formulaSize _).head, fragments.toSet)
        }

      info("")
      info("")
      info("")
      info("sorted:\n" + sorted.
        map({ case (k, v, _) => k.head + "\n" + v }).mkString("\n******\n"))
        
      info("fragments set from sorted:\n" + sorted.map(_._2).toSet.mkString("\n"))
      
//      info("unordered fragments set:\n" + (transformedExamples zip unorderedFragments).
//        map({ case (k, v) => k._1.head + "\n" + k._2 + "\n" + v }).mkString("\n******\n"))
//      info("unordered fragments set:\n" + unorderedFragments.toSet.mkString("\n\n\n"))
        
      val inputsPerPredicate =
        for ((examplePair, fragment) <- zipped) yield {
          val (_, fragmentHead, _) = sorted.find(_._3 contains fragment).get
          
          (fragmentHead, examplePair)
        }

      val inputsPerPredicateMap =
        (Map[Expr, Set[InputOutputExampleVal]]() /: inputsPerPredicate) {
          case (current, (fragment, pair)) =>
            current + (fragment -> (current.getOrElse(fragment, Set[InputOutputExampleVal]()) + pair))
        }
      
      inputsPerPredicateMap.size shouldBe 4
      
      
      // FIXME make this a separate test
      {
        val fragments = inputsPerPredicateMap.keys
        
        val synthesizer = new Synthesizer
       
        val allDiffResults =
    	    (for((f1, f2) <- fragments zip fragments.tail) yield {
    	      val diffs = Differencer.differences(f1, f2, problem.as.map(_.toVariable))
    	      info(s"diffs for $f1 and $f2 are $diffs")
    	      (f1, f2, diffs)
    	    })
        info("allDiffResults: " + allDiffResults.map(_._3.map({ case (k, v) => (k, v(w))})))
        
        allDiffResults.map(_._3).flatten should be ('empty)
      }
      
      // FIXME make this a separate test
//      {
//        val fragments = inputsPerPredicateMap.keys.toList
//        
//        val synthesizer = new Synthesizer
//       
//        val (empty, nonEmpty) = synthesizer.calculateFragments(fragments, problem.as.map(_.toVariable))
//        
//        info("(empty, nonEmpty): " + (empty, nonEmpty))
//        
//        empty shouldBe ('empty)
//        nonEmpty shouldBe ('empty)
//        
//        ???
//      }
      
      
//      info("inputsPerPredicateMap:\n" + inputsPerPredicateMap.mkString("\n"))
      
      val intersections =
        for ((fragment, pairs) <- inputsPerPredicateMap) yield {
          
          val subexpressionsSets =
            for ((inputs, _) <- pairs) yield         
              ioexamples.Util.subexpressionToPathFunctionsPairs(inputs.head).map({ case (k, v) => (k, v(w)) }).toSet
              
          info("subexpressionsSets:\n" + subexpressionsSets.mkString("\n"))
          
          subexpressionsSets.size shouldBe 9
          
          // NOTE paper, input size of the tree
          ExprOps.formulaSize(pairs.head._1.head) shouldBe 7
          
          val intersection =
            subexpressionsSets.reduce(_ intersect _)
            
          info("intersection:\n" + intersection.mkString("\n"))
          
          intersection
        }
      
//      val groupedBySize =
//        for (intersection <- intersections) yield {
//          
////          val diffed =
////            (intersection /: intersections) {
////              case (curr,  intersectionToRemove) if intersection != intersectionToRemove =>
////                curr diff intersectionToRemove
////              case (curr, _) =>
////                curr
////            }
////          
////          info("diffed: " + diffed)
//
//          intersection.map( x => (x, ExprOps.formulaSize(x._2)) ).groupBy(_._2)
//        }
//
//      info("groupedBySize: " + groupedBySize.mkString("\n"))
      
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
      
    }

  }
  
  test("use synthesizer") {
    val eb = problem.eb

    info("invalids:\n" + eb.invalids.mkString("\n"))
    info("valids:\n" + eb.valids.mkString("\n"))

    problem.hasOutputTests shouldBe false

    problem.xs should have size 1

    val resType = problem.xs.head.getType

    val start1 = System.currentTimeMillis()
    val ms = new scope.AccumulatingScope
    val enum = constructEnumerator_new(ms)

    info("going into enumeration")

    val firstNNormal = (
      for (size <- 4 to 8) yield {
        for (
          blackHeight <- 1 to (Math.log2(size + 1).toInt + 1);
          e = enum.getEnum(size, 1 to size, 0 to 1, blackHeight);
          ind <- 0 until e.size
        ) yield e(ind)
      }).flatten
      
    info("enumerated datastructs: " + firstNNormal.mkString("\n"))
        
    val firstNReverted =
      for (
        tree <- firstNNormal
      ) yield {
        tree match {
          case
            CaseClass(`nodeClass`,
              `black` ::
              CaseClass(`nodeClass`,
                `red` :: ll :: lv :: lr :: Nil) ::
              v ::
              CaseClass(`nodeClass`,
                `red` :: rl :: rv :: rr :: Nil) ::
            Nil) =>
              val v1 =
                CaseClass(nodeClass,
                  black ::
                  CaseClass(nodeClass,
                    red ::
                    CaseClass(nodeClass,
                      red :: ll :: lv :: lr :: Nil) ::
                    v :: rl :: Nil) ::
                  rv ::
                  rr ::
                Nil)
                
              val v2 =
                CaseClass(nodeClass,
                  black ::
                  CaseClass(nodeClass,
                    red :: ll :: lv ::
                    CaseClass(nodeClass,
                      red :: lr :: v :: rl :: Nil) :: Nil) ::
                  rv ::
                  rr ::
                Nil)
                
              val v3 =
                CaseClass(nodeClass,
                  black ::
                  ll ::
                  lv ::
                  CaseClass(nodeClass,
                    red ::
                    CaseClass(nodeClass,
                      red :: lr :: v :: rl :: Nil) ::
                    rv :: rr :: Nil) ::
                Nil)
                
              val v4 =
                CaseClass(nodeClass,
                  black ::
                  ll ::
                  lv ::
                  CaseClass(nodeClass,
                    red :: lr :: v ::
                    CaseClass(nodeClass,
                      red :: rl :: rv :: rr :: Nil) :: Nil) ::
                Nil)
                
              v1 :: v2 :: v3 :: v4 :: Nil
          case _ =>
            Nil
        }
      }

    val firstN =
      firstNNormal ++
      firstNReverted.flatten

    val numOfExamples = 42
      
    info("firstN:\n" + firstN.zipWithIndex.mkString("\n"))
//    firstN.size shouldBe numOfExamples

    val filteredExamples =
    {
      val phi = problem.phi

      problem.as should have size 1
      val in = problem.as.head
      problem.xs should have size 1
      val out = problem.xs.head
      val pc = problem.pc
      val toEvaluate = pc.toClause
      val compiled = evaluator.compile(toEvaluate, in :: Nil).get

      for (ex1 <- firstN) yield {
        val res = compiled(new Model(Map(in -> ex1)))

        res match {
          case EvaluationResults.Successful(BooleanLiteral(v)) if v =>
            info("pass precondition for: " + ex1)
            Some(ex1)
          case e: EvaluationResults.EvaluatorError =>
            None
          case _                                   =>
            None
        }
      }

    }

    //    "output finding" -
    {
      val phi = problem.phi

      problem.as should have size 1
      val in = problem.as.head

      problem.xs should have size 1
      val out = problem.xs.head

//      val results = collection.mutable.Map[Expr, Expr]()//.withDefaultValue(Set())     
//      val results = collection.mutable.Map[Expr, Set[Expr]]().withDefaultValue(Set())     
      val results = new collection.mutable.MutableList[(Expr, Expr)]()

//      import scala.util.Random
//      val randomGen = new Random("random number my version".hashCode)
//      val randoms = Seq.fill(300)(randomGen.nextInt(numOfExamples))
//      info("randoms " + randoms)

      val pc = problem.pc
      val toEvaluate = And(pc.toClause, phi)
      val compiled = evaluator.compile(toEvaluate, in :: out :: Nil).get
            
      filteredExamples.flatten.size shouldBe 36
//      info(filteredExamples.map(p => "in : " + p).mkString("\n"))

//      for (ex1 <- firstN) {
      for (ex1 <- filteredExamples.flatten) {
        //        _ = info("*******");
        var flag = true
        val ex2it = firstN.iterator;
        while (flag && ex2it.nonEmpty) {
          val ex2 = ex2it.next
          //        info("toEvaluate " + toEvaluate)

          //        info(s"for in $ex1, out $ex2")
          //          val res = evaluator.eval(toEvaluate, new Model(Map(in -> ex1, out -> ex2)))
          val res = compiled(new Model(Map(in -> ex1, out -> ex2)))
          //          info(s"for in $ex1, out $ex2, got $res")

          res match {
            case EvaluationResults.Successful(BooleanLiteral(v)) if v =>
              //            info(s"for in $ex1, out $ex2")
              //            info("***")
              //              info(s"for input $ex1\n, output $ex2\n existing result is ${results.getOrElse(ex1, w)}\n") 
              //              withClue(s"for input $ex1\n, output $ex2\n existing result is ${results.getOrElse(ex1, w)}\n") {
              //                results.getOrElse(ex1, ex2) shouldBe ex2
              //              results should not contain key (ex1)
              //              }

//              assert(!(results contains ex1))
//              assert(results.getOrElse(ex1, ex2))
//              results(ex1) += ex2
              results += ((ex1, ex2))
            //          info(s"$v for $ex, ${v1}, $v2")
            case e: EvaluationResults.EvaluatorError =>
            //          info("evaluation failure: " + e + s" for $v1 and $v2")
            case _                                   =>
          }
        }
      }

      results.size shouldBe >(0)
//      info(results.map(p => "in : " + p._1 + "\nout: " + p._2).mkString("\n"))
//      results should have size (56)

//      info(s"result with more than 6 results: " + {
//        val (k, v) = results.groupBy(_._1).find(_._2.size > 1).get
//        val v2 = v.map({ case (k, v) => v })
//        k + "\n" + v2
//      })

      val extraction = new ExamplesExtraction(sctx, sctx.program)
      
//      val resultsSmallest = results map {
//        case (k, v) =>
//          (k, v.toList.sortBy(ExprOps.formulaSize _).head)
//      }

      val examples = results.map({
        case (inEx, outEx) =>
          ((in, inEx) :: Nil, (out, outEx))
      }).toList

      //      info("examples\n" + examples.mkString("\n"))
//      examples should have size (56)

      //      test: attempt synthesis
      // get fragments
      val ((inIds, outId), transformedExamples) = ExamplesExtraction.transformMappings(examples).get
      info(s"inIds $inIds")
      info("transformed examples: " + transformedExamples.mkString("\n"))
      val unorderedFragments = Fragmenter.constructFragments(transformedExamples, inIds)
      info("unordered fragments: " + unorderedFragments.mkString("\n"))
      
      val unorederdFragmentsSet = unorderedFragments.toSet
      info("unorederdFragmentsSet:\n" + unorederdFragmentsSet.mkString("\n"))
      
      transformedExamples should have size unorderedFragments.size
      val zipped = (examples zip unorderedFragments)
      
      val zippedSorted = zipped.sortBy(p => ExprOps.formulaSize(p._2))
      
//      info("" + ioexamples.Util.mapOfSubexpressionsToPathFunctions(zippedSorted.last._1._1.head).map(
//        {case (k,v) => "" + k + "\n" + v(w) }))
//      info("" + zippedSorted.last)
      
      val taken = new collection.mutable.ListBuffer[((List[Expressions.Expr], Expressions.Expr), Expressions.Expr)]()
      var covering = transformedExamples.map(_._1).toSet

//      val coveringPairs =
//        zippedSorted.takeWhile({
//          case _ if covering.isEmpty =>
//            false
//          case p@(exPair, fragment) =>
//            covering = covering - exPair._1
//            taken += p
//            true
//        })
//        
//      info(s"zipped size ${zipped.size}; coveringPairs size: " + coveringPairs.size)
//      info("covering pairs\n: " + coveringPairs.mkString("\n"))
      
      val groupped = zipped.groupBy(_._1._1)
      val sorted =
        for ( (input, list) <- groupped ) yield {
          val fragments = list.map(_._2)
          (input, fragments.sortBy(ExprOps.formulaSize _).head, fragments.toSet)
        }
      
      
      val start2 = System.currentTimeMillis() - start1
      println("start2: " + start2)

      info("")
      info("")
      info("")
      info("sorted:\n" + sorted.
        map({ case (k, v, _) => k.head + "\n" + v }).mkString("\n******\n"))
        
      info("fragments set from sorted:\n" + sorted.map(_._2).toSet.mkString("\n"))
      
//      info("unordered fragments set:\n" + (transformedExamples zip unorderedFragments).
//        map({ case (k, v) => k._1.head + "\n" + k._2 + "\n" + v }).mkString("\n******\n"))
//      info("unordered fragments set:\n" + unorderedFragments.toSet.mkString("\n\n\n"))
        
      val inputsPerPredicate =
        for ((examplePair, fragment) <- zipped) yield {
          val (_, fragmentHead, _) = sorted.find(_._3 contains fragment).get
          
          (fragmentHead, examplePair)
        }

      val inputsPerPredicateMap =
        (Map[Expr, Set[InputOutputExample]]() /: inputsPerPredicate) {
          case (current, (fragment, pair)) =>
            current + (fragment -> (current.getOrElse(fragment, Set[InputOutputExample]()) + pair))
        }
      
      inputsPerPredicateMap.size shouldBe 4
      
      val synthesizer = new Synthesizer
      
//      val resultPredicates =
//        synthesizer.calculatePredicatesStructureMapDifference(inputsPerPredicateMap.toSeq)
//
//      info("res: " + resultPredicates)
      {
        
        val (examples, fragments) =
          (for (fragment <- inputsPerPredicateMap.keys.toList;
            example <- inputsPerPredicateMap(fragment).toList) yield
            (example, fragment)).unzip
            
        info("examples: " + examples)
        info("fragments: " + fragments)
      
        val result =
          synthesizer.synthesize(examples.toList, getEnum, evaluator, program.caseClassDef("Empty").typed, Some(fragments.toList))
          
        val start3 = System.currentTimeMillis() - start1
        println("start3: " + start3)
          
        info("result: " + result)
        
        result.get._1.toString shouldBe
          """|if (Red == t.left.color && Black == t.left.right.color) {
             |  Node(t.left.color, Node(t.color, t.left.left.left, t.left.left.value, t.left.left.right), t.left.value, Node(t.color, t.left.right, t.value, t.right))
             |} else if (Red == t.left.color && Black != t.left.right.color) {
             |  Node(t.left.color, Node(t.color, t.left.left, t.left.value, t.left.right.left), t.left.right.value, Node(t.color, t.left.right.right, t.value, t.right))
             |} else if (Red != t.left.color && Red == t.right.left.color) {
             |  Node(t.right.color, Node(t.color, t.left, t.value, t.right.left.left), t.right.left.value, Node(t.color, t.right.left.right, t.right.value, t.right.right))
             |} else if (Red != t.left.color && Red != t.right.left.color) {
             |  Node(t.right.color, Node(t.color, t.left, t.value, t.right.left), t.right.value, Node(t.color, t.right.right.left, t.right.right.value, t.right.right.right))
             |} else {
             |  ()
             |}""".stripMargin
      }
    }

  }
  
//
//
//
//
//  ignore("example extraction") {
//    val eb = problem.eb
//
//    info("invalids:\n" + eb.invalids.mkString("\n"))
//    info("valids:\n" + eb.valids.mkString("\n"))
//
//    problem.hasOutputTests shouldBe false
//
//    problem.xs should have size 1
//
//    val resType = problem.xs.head.getType
//
//    val enum = getEnum(resType)
//
//    val firstN = enum.take(2000).toArray.sortBy(_.hashCode)
//
//    val numOfExamples = 1152
//
//    info("firstN:\n" + firstN.zipWithIndex.mkString("\n"))
//    firstN.size shouldBe numOfExamples
//
//    //    "output finding" -
//    {
//      val phi = problem.phi
//
//      problem.as should have size 1
//      val in = problem.as.head
//
//      problem.xs should have size 1
//      val out = problem.xs.head
//
//      //      val results = collection.mutable.Map[Expr, Set[Expr]]().withDefaultValue(Set())     
//      val results = new collection.mutable.MutableList[(Expr, Expr)]()
//
//      import scala.util.Random
//      val randomGen = new Random("random number my version".hashCode)
//      val randoms = Seq.fill(300)(randomGen.nextInt(numOfExamples))
//      info("randoms " + randoms)
//
//      val pc = problem.pc
//      val toEvaluate = And(pc.toClause, phi)
//      val compiled = evaluator.compile(toEvaluate, in :: out :: Nil).get
//
//      for (ex1 <- randoms map { firstN }) {
//        //        _ = info("*******");
//        var flag = true
//        val ex2it = firstN.iterator;
//        while (flag && ex2it.nonEmpty) {
//          val ex2 = ex2it.next
//          //        info("toEvaluate " + toEvaluate)
//
//          //        info(s"for in $ex1, out $ex2")
//          //          val res = evaluator.eval(toEvaluate, new Model(Map(in -> ex1, out -> ex2)))
//          val res = compiled(new Model(Map(in -> ex1, out -> ex2)))
//          //          info(s"for in $ex1, out $ex2, got $res")
//
//          res match {
//            case EvaluationResults.Successful(BooleanLiteral(v)) if v =>
//              //            info(s"for in $ex1, out $ex2")
//              //            info("***")
//              //              info(s"for input $ex1\n, output $ex2\n existing result is ${results.getOrElse(ex1, w)}\n") 
//              //              withClue(s"for input $ex1\n, output $ex2\n existing result is ${results.getOrElse(ex1, w)}\n") {
//              //                results.getOrElse(ex1, ex2) shouldBe ex2
//              //              results should not contain key (ex1)
//              //              }
//
//              //              results(ex1) += ex2
//              results += ((ex1, ex2))
//            //          info(s"$v for $ex, ${v1}, $v2")
//            case e: EvaluationResults.EvaluatorError =>
//            //          info("evaluation failure: " + e + s" for $v1 and $v2")
//            case _                                   =>
//          }
//        }
//      }
//
//      results.size shouldBe >(0)
//      results should have size (96)
//
//      info(s"result with more than 6 results: " + {
//        val (k, v) = results.groupBy(_._1).find(_._2.size > 2).get
//        val v2 = v.map({ case (k, v) => v })
//        k + "\n" + v2
//      })
//
//      val extraction = new ExamplesExtraction(sctx, sctx.program)
//
//      val examples = results.map({
//        case (inEx, outEx) =>
//          ((in, inEx) :: Nil, (out, outEx))
//      }).toList
//
//      //      info("examples\n" + examples.mkString("\n"))
//      examples should have size (96)
//
//      //      test: attempt synthesis
//      // get fragments
//      //      val ((inIds, outId), transformedExamples) = ExamplesExtraction.transformMappings(examples).get
//      //      info(s"inIds $inIds")
//      //      info("transformed examples: " + transformedExamples.mkString("\n"))
//      //      val unorderedFragments = Fragmenter.constructFragments(transformedExamples, inIds)
//      //      info("unordered fragments: " + unorderedFragments.mkString("\n"))
//
//    }
//
//  }
//  
//  ignore("synthesis, two cases") {
//    val eb = problem.eb
//
//    info("invalids:\n" + eb.invalids.mkString("\n"))
//    info("valids:\n" + eb.valids.mkString("\n"))
//
//    problem.hasOutputTests shouldBe false
//
//    problem.xs should have size 1
//
//    val resType = problem.xs.head.getType
//
//    val ms = new scope.AccumulatingScope
//    val enum = constructEnumerator_new(ms)
//
//    info("going into enumeration")
//
//    val firstNNormal = (
//      for (size <- 3 to 7) yield {
//        for (
//          blackHeight <- 1 to (Math.log2(size + 1).toInt + 1);
//          e = enum.getEnum(size, 1 to size, 0 to 1, blackHeight);
//          ind <- 0 until e.size
//        ) yield e(ind)
//      }).flatten
//      
//    info("enumerated datastructs: " + firstNNormal.mkString("\n"))
//        
//    val firstNReverted =
//      for (
//        tree <- firstNNormal
//      ) yield {
//        tree match {
////          case
////            CaseClass(_,
////              `black` ::
////              CaseClass(_,
////                `red` :: ll :: lv :: lr :: Nil) ::
////              v ::
////              CaseClass(_,
////                `red` :: rl :: rv :: rr :: Nil) ::
////            Nil) =>
////              ???
//          case
//            CaseClass(`nodeClass`,
//              `black` ::
//              CaseClass(`nodeClass`,
//                `red` :: ll :: lv :: lr :: Nil) ::
//              v ::
//              CaseClass(`nodeClass`,
//                `red` :: rl :: rv :: rr :: Nil) ::
//            Nil) =>
//              val v1 =
//                CaseClass(nodeClass,
//                  black ::
//                  CaseClass(nodeClass,
//                    red ::
//                    CaseClass(nodeClass,
//                      red :: ll :: lv :: lr :: Nil) ::
//                    v :: rl :: Nil) ::
//                  rv ::
//                  rr ::
//                Nil)
//                
//              val v2 =
//                CaseClass(nodeClass,
//                  black ::
//                  CaseClass(nodeClass,
//                    red :: ll :: lv ::
//                    CaseClass(nodeClass,
//                      red :: lr :: v :: rl :: Nil) :: Nil) ::
//                  rv ::
//                  rr ::
//                Nil)
//                
////              val v3 =
////                CaseClass(nodeClass,
////                  ??? ::
////                  CaseClass(nodeClass,
////                    ??? ::
////                    CaseClass(nodeClass,
////                      ??? :: ??? :: ??? :: ??? :: Nil) ::
////                    ??? :: ??? :: Nil) ::
////                  ??? ::
////                  ??? ::
////                Nil)
////                
////                
////              val v4 =
////                CaseClass(nodeClass,
////                  ??? ::
////                  CaseClass(nodeClass,
////                    ??? ::
////                    CaseClass(nodeClass,
////                      ??? :: ??? :: ??? :: ??? :: Nil) ::
////                    ??? :: ??? :: Nil) ::
////                  ??? ::
////                  ??? ::
////                Nil)
////                
//              v1 :: v2 :: Nil // v3 :: v4 :: Nil
//          case _ =>
//            Nil
//        }
//      }
//
////      only red to black
////      firstNNormal map {
////        case CaseClass(nodeClass, color :: leftTree :: IntLiteral(currRoot) :: rightTree :: Nil) =>
////          if (color == red)
////            CaseClass(nodeClass, black :: leftTree :: IntLiteral(currRoot) :: rightTree :: Nil)
////          else
////            CaseClass(nodeClass, red :: leftTree :: IntLiteral(currRoot) :: rightTree :: Nil)
////      }
//    
//    val firstN =
//      firstNNormal ++
//      firstNReverted.flatten
//
//    val numOfExamples = 42
//      
//    info("firstN:\n" + firstN.zipWithIndex.mkString("\n"))
////    firstN.size shouldBe numOfExamples
//
//    val filteredExamples =
//    {
//      val phi = problem.phi
//
//      problem.as should have size 1
//      val in = problem.as.head
//      problem.xs should have size 1
//      val out = problem.xs.head
//      val pc = problem.pc
//      val toEvaluate = pc.toClause
//      val compiled = evaluator.compile(toEvaluate, in :: Nil).get
//
//      for (ex1 <- firstN) yield {
//        //        _ = info("*******");
//        val res = compiled(new Model(Map(in -> ex1)))
//        //          info(s"for in $ex1, out $ex2, got $res")
//
//        res match {
//          case EvaluationResults.Successful(BooleanLiteral(v)) if v =>
//            //            info(s"for in $ex1, out $ex2")
//            //            info("***")
//            //              info(s"for input $ex1\n, output $ex2\n existing result is ${results.getOrElse(ex1, w)}\n") 
//            //              withClue(s"for input $ex1\n, output $ex2\n existing result is ${results.getOrElse(ex1, w)}\n") {
//            //                results.getOrElse(ex1, ex2) shouldBe ex2
//            //              results should not contain key (ex1)
//            //              }
//
////              assert(!(results contains ex1))
////              assert(results.getOrElse(ex1, ex2))
////              results(ex1) = ex2
//            info("pass precondition for: " + ex1)
//            Some(ex1)
//          //          info(s"$v for $ex, ${v1}, $v2")
//          case e: EvaluationResults.EvaluatorError =>
////                    info("evaluation failure: " + e + s" for $ex1")
//            None
//          case _                                   =>
////            info("ffs")
//            None
//        }
//      }
//
//    }
//
//    //    "output finding" -
//    {
//      val phi = problem.phi
//
//      problem.as should have size 1
//      val in = problem.as.head
//
//      problem.xs should have size 1
//      val out = problem.xs.head
//
////      val results = collection.mutable.Map[Expr, Expr]()//.withDefaultValue(Set())     
////      val results = collection.mutable.Map[Expr, Set[Expr]]().withDefaultValue(Set())     
//      val results = new collection.mutable.MutableList[(Expr, Expr)]()
//
////      import scala.util.Random
////      val randomGen = new Random("random number my version".hashCode)
////      val randoms = Seq.fill(300)(randomGen.nextInt(numOfExamples))
////      info("randoms " + randoms)
//
//      val pc = problem.pc
//      val toEvaluate = And(pc.toClause, phi)
//      val compiled = evaluator.compile(toEvaluate, in :: out :: Nil).get
//
////      for (ex1 <- firstN) {
//      for (ex1 <- filteredExamples.flatten) {
//        //        _ = info("*******");
//        var flag = true
//        val ex2it = firstN.iterator;
//        while (flag && ex2it.nonEmpty) {
//          val ex2 = ex2it.next
//          //        info("toEvaluate " + toEvaluate)
//
//          //        info(s"for in $ex1, out $ex2")
//          //          val res = evaluator.eval(toEvaluate, new Model(Map(in -> ex1, out -> ex2)))
//          val res = compiled(new Model(Map(in -> ex1, out -> ex2)))
//          //          info(s"for in $ex1, out $ex2, got $res")
//
//          res match {
//            case EvaluationResults.Successful(BooleanLiteral(v)) if v =>
//              //            info(s"for in $ex1, out $ex2")
//              //            info("***")
//              //              info(s"for input $ex1\n, output $ex2\n existing result is ${results.getOrElse(ex1, w)}\n") 
//              //              withClue(s"for input $ex1\n, output $ex2\n existing result is ${results.getOrElse(ex1, w)}\n") {
//              //                results.getOrElse(ex1, ex2) shouldBe ex2
//              //              results should not contain key (ex1)
//              //              }
//
////              assert(!(results contains ex1))
////              assert(results.getOrElse(ex1, ex2))
////              results(ex1) += ex2
//              results += ((ex1, ex2))
//            //          info(s"$v for $ex, ${v1}, $v2")
//            case e: EvaluationResults.EvaluatorError =>
//            //          info("evaluation failure: " + e + s" for $v1 and $v2")
//            case _                                   =>
//          }
//        }
//      }
//
//      results.size shouldBe >(0)
////      results should have size (56)
//
////      info(s"result with more than 6 results: " + {
////        val (k, v) = results.groupBy(_._1).find(_._2.size > 1).get
////        val v2 = v.map({ case (k, v) => v })
////        k + "\n" + v2
////      })
//
//      val extraction = new ExamplesExtraction(sctx, sctx.program)
//      
////      val resultsSmallest = results map {
////        case (k, v) =>
////          (k, v.toList.sortBy(ExprOps.formulaSize _).head)
////      }
//
//      val examples = results.map({
//        case (inEx, outEx) =>
//          ((in, inEx) :: Nil, (out, outEx))
//      }).toList
//
//      //      info("examples\n" + examples.mkString("\n"))
////      examples should have size (56)
//
//      //      test: attempt synthesis
//      // get fragments
//      val ((inIds, outId), transformedExamples) = ExamplesExtraction.transformMappings(examples).get
//      info(s"inIds $inIds")
//      info("transformed examples: " + transformedExamples.mkString("\n"))
//      val unorderedFragments = Fragmenter.constructFragments(transformedExamples, inIds)
//      info("unordered fragments: " + unorderedFragments.mkString("\n"))
//      
//      transformedExamples should have size unorderedFragments.size
//      val zipped = (transformedExamples zip unorderedFragments)
//      val groupped = zipped.groupBy(_._1._1)
//      val sorted =
//        for ( (input, list) <- groupped ) yield {
//          val fragments = list.map(_._2)
//          (input, fragments.sortBy(ExprOps.formulaSize _).head)
//        }
//
//      info("")
//      info("")
//      info("")
//      info("sorted:\n" + sorted.
//        map({ case (k, v) => k.head + "\n" + v }).mkString("\n******\n"))
////      info("unordered fragments set:\n" + (transformedExamples zip unorderedFragments).
////        map({ case (k, v) => k._1.head + "\n" + k._2 + "\n" + v }).mkString("\n******\n"))
////      info("unordered fragments set:\n" + unorderedFragments.toSet.mkString("\n\n\n"))
//
//    }
//
//  }
//  
//  ignore("datageneration, with precondition") {
//    val eb = problem.eb
//
//    info("invalids:\n" + eb.invalids.mkString("\n"))
//    info("valids:\n" + eb.valids.mkString("\n"))
//
//    problem.hasOutputTests shouldBe false
//
//    problem.xs should have size 1
//
//    val resType = problem.xs.head.getType
//
//    val ms = new scope.AccumulatingScope
//    val enum = constructEnumerator_new(ms)
//
//    info("going into enumeration")
//
//    val firstNNormal = (
//      for (size <- 3 to 7) yield {
//        for (
//          blackHeight <- 1 to (Math.log2(size + 1).toInt + 1);
//          e = enum.getEnum(size, 1 to size, 0 to 1, blackHeight);
//          ind <- 0 until e.size
//        ) yield e(ind)
//      }).flatten
//      
//    info("enumerated datastructs: " + firstNNormal.mkString("\n"))
//        
//    val firstNReverted =
//      for (
//        tree <- firstNNormal
//      ) yield {
//        tree match {
////          case
////            CaseClass(_,
////              `black` ::
////              CaseClass(_,
////                `red` :: ll :: lv :: lr :: Nil) ::
////              v ::
////              CaseClass(_,
////                `red` :: rl :: rv :: rr :: Nil) ::
////            Nil) =>
////              ???
//          case
//            CaseClass(`nodeClass`,
//              `black` ::
//              CaseClass(`nodeClass`,
//                `red` :: ll :: lv :: lr :: Nil) ::
//              v ::
//              CaseClass(`nodeClass`,
//                `red` :: rl :: rv :: rr :: Nil) ::
//            Nil) =>
//              val v1 =
//                CaseClass(nodeClass,
//                  black ::
//                  CaseClass(nodeClass,
//                    red ::
//                    CaseClass(nodeClass,
//                      red :: ll :: lv :: lr :: Nil) ::
//                    v :: rl :: Nil) ::
//                  rv ::
//                  rr ::
//                Nil)
//                
//              val v2 =
//                CaseClass(nodeClass,
//                  black ::
//                  CaseClass(nodeClass,
//                    red :: ll :: lv ::
//                    CaseClass(nodeClass,
//                      red :: lr :: v :: rl :: Nil) :: Nil) ::
//                  rv ::
//                  rr ::
//                Nil)
//                
//              val v3 =
//                CaseClass(nodeClass,
//                  black ::
//                  ll ::
//                  lv ::
//                  CaseClass(nodeClass,
//                    red ::
//                    CaseClass(nodeClass,
//                      red :: lr :: v :: rl :: Nil) ::
//                    rv :: rr :: Nil) ::
//                Nil)
//                
//              val v4 =
//                CaseClass(nodeClass,
//                  black ::
//                  ll ::
//                  lv ::
//                  CaseClass(nodeClass,
//                    red :: lr :: v ::
//                    CaseClass(nodeClass,
//                      red :: rl :: rv :: rr :: Nil) :: Nil) ::
//                Nil)
//                
//              v1 :: v2 :: v3 :: v4 :: Nil
//          case _ =>
//            Nil
//        }
//      }
//
////      only red to black
////      firstNNormal map {
////        case CaseClass(nodeClass, color :: leftTree :: IntLiteral(currRoot) :: rightTree :: Nil) =>
////          if (color == red)
////            CaseClass(nodeClass, black :: leftTree :: IntLiteral(currRoot) :: rightTree :: Nil)
////          else
////            CaseClass(nodeClass, red :: leftTree :: IntLiteral(currRoot) :: rightTree :: Nil)
////      }
//    
//    val firstN =
//      firstNNormal ++
//      firstNReverted.flatten
//
//    val numOfExamples = 42
//      
//    info("firstN:\n" + firstN.zipWithIndex.mkString("\n"))
////    firstN.size shouldBe numOfExamples
//
//    val phi = problem.phi
//
//    problem.as should have size 1
//    val in = problem.as.head
//    problem.xs should have size 1
//    val out = problem.xs.head
//    val pc = problem.pc
//    info("precondition to evaluate: " + pc)
//    val toEvaluate = pc.toClause
//    val compiled = evaluator.compile(toEvaluate, in :: Nil).get
//
//    for (ex1 <- firstN) {
//      val res = compiled(new Model(Map(in -> ex1)))
//
//      res match {
//        case EvaluationResults.Successful(BooleanLiteral(v)) =>
//          info("pass precondition for: " + ex1)
//          
//          v shouldBe (firstNReverted.flatten contains ex1)
//          v shouldBe !(firstNNormal contains ex1)
//        case e: EvaluationResults.EvaluatorError =>
//          None
//        case _                                   =>
//          None
//      }
//    }
//
//  }
//  
//  ignore("synthesis") {
//    val eb = problem.eb
//
//    info("invalids:\n" + eb.invalids.mkString("\n"))
//    info("valids:\n" + eb.valids.mkString("\n"))
//
//    problem.hasOutputTests shouldBe false
//
//    problem.xs should have size 1
//
//    val resType = problem.xs.head.getType
//
//    val ms = new scope.AccumulatingScope
//    val enum = constructEnumerator_new(ms)
//
//    info("going into enumeration")
//
//    val firstNNormal = (
//      for (size <- 3 to 7) yield {
//        for (
//          blackHeight <- 1 to (Math.log2(size + 1).toInt + 1);
//          e = enum.getEnum(size, 1 to size, 0 to 1, blackHeight);
//          ind <- 0 until e.size
//        ) yield e(ind)
//      }).flatten
//      
//    info("enumerated datastructs: " + firstNNormal.mkString("\n"))
//        
//    val firstNReverted =
//      for (
//        tree <- firstNNormal
//      ) yield {
//        tree match {
//          case
//            CaseClass(`nodeClass`,
//              `black` ::
//              CaseClass(`nodeClass`,
//                `red` :: ll :: lv :: lr :: Nil) ::
//              v ::
//              CaseClass(`nodeClass`,
//                `red` :: rl :: rv :: rr :: Nil) ::
//            Nil) =>
//              val v1 =
//                CaseClass(nodeClass,
//                  black ::
//                  CaseClass(nodeClass,
//                    red ::
//                    CaseClass(nodeClass,
//                      red :: ll :: lv :: lr :: Nil) ::
//                    v :: rl :: Nil) ::
//                  rv ::
//                  rr ::
//                Nil)
//                
//              val v2 =
//                CaseClass(nodeClass,
//                  black ::
//                  CaseClass(nodeClass,
//                    red :: ll :: lv ::
//                    CaseClass(nodeClass,
//                      red :: lr :: v :: rl :: Nil) :: Nil) ::
//                  rv ::
//                  rr ::
//                Nil)
//                
//              val v3 =
//                CaseClass(nodeClass,
//                  black ::
//                  ll ::
//                  lv ::
//                  CaseClass(nodeClass,
//                    red ::
//                    CaseClass(nodeClass,
//                      red :: lr :: v :: rl :: Nil) ::
//                    rv :: rr :: Nil) ::
//                Nil)
//                
//              val v4 =
//                CaseClass(nodeClass,
//                  black ::
//                  ll ::
//                  lv ::
//                  CaseClass(nodeClass,
//                    red :: lr :: v ::
//                    CaseClass(nodeClass,
//                      red :: rl :: rv :: rr :: Nil) :: Nil) ::
//                Nil)
//                
//              v1 :: v2 :: v3 :: v4 :: Nil
//          case _ =>
//            Nil
//        }
//      }
//
//    val firstN =
//      firstNNormal ++
//      firstNReverted.flatten
//
//    val numOfExamples = 42
//      
//    info("firstN:\n" + firstN.zipWithIndex.mkString("\n"))
////    firstN.size shouldBe numOfExamples
//
//    val filteredExamples =
//    {
//      val phi = problem.phi
//
//      problem.as should have size 1
//      val in = problem.as.head
//      problem.xs should have size 1
//      val out = problem.xs.head
//      val pc = problem.pc
//      val toEvaluate = pc.toClause
//      val compiled = evaluator.compile(toEvaluate, in :: Nil).get
//
//      for (ex1 <- firstN) yield {
//        val res = compiled(new Model(Map(in -> ex1)))
//
//        res match {
//          case EvaluationResults.Successful(BooleanLiteral(v)) if v =>
//            info("pass precondition for: " + ex1)
//            Some(ex1)
//          case e: EvaluationResults.EvaluatorError =>
//            None
//          case _                                   =>
//            None
//        }
//      }
//
//    }
//
//    //    "output finding" -
//    {
//      val phi = problem.phi
//
//      problem.as should have size 1
//      val in = problem.as.head
//
//      problem.xs should have size 1
//      val out = problem.xs.head
//
////      val results = collection.mutable.Map[Expr, Expr]()//.withDefaultValue(Set())     
////      val results = collection.mutable.Map[Expr, Set[Expr]]().withDefaultValue(Set())     
//      val results = new collection.mutable.MutableList[(Expr, Expr)]()
//
////      import scala.util.Random
////      val randomGen = new Random("random number my version".hashCode)
////      val randoms = Seq.fill(300)(randomGen.nextInt(numOfExamples))
////      info("randoms " + randoms)
//
//      val pc = problem.pc
//      val toEvaluate = And(pc.toClause, phi)
//      val compiled = evaluator.compile(toEvaluate, in :: out :: Nil).get
//
////      for (ex1 <- firstN) {
//      for (ex1 <- filteredExamples.flatten) {
//        //        _ = info("*******");
//        var flag = true
//        val ex2it = firstN.iterator;
//        while (flag && ex2it.nonEmpty) {
//          val ex2 = ex2it.next
//          //        info("toEvaluate " + toEvaluate)
//
//          //        info(s"for in $ex1, out $ex2")
//          //          val res = evaluator.eval(toEvaluate, new Model(Map(in -> ex1, out -> ex2)))
//          val res = compiled(new Model(Map(in -> ex1, out -> ex2)))
//          //          info(s"for in $ex1, out $ex2, got $res")
//
//          res match {
//            case EvaluationResults.Successful(BooleanLiteral(v)) if v =>
//              //            info(s"for in $ex1, out $ex2")
//              //            info("***")
//              //              info(s"for input $ex1\n, output $ex2\n existing result is ${results.getOrElse(ex1, w)}\n") 
//              //              withClue(s"for input $ex1\n, output $ex2\n existing result is ${results.getOrElse(ex1, w)}\n") {
//              //                results.getOrElse(ex1, ex2) shouldBe ex2
//              //              results should not contain key (ex1)
//              //              }
//
////              assert(!(results contains ex1))
////              assert(results.getOrElse(ex1, ex2))
////              results(ex1) += ex2
//              results += ((ex1, ex2))
//            //          info(s"$v for $ex, ${v1}, $v2")
//            case e: EvaluationResults.EvaluatorError =>
//            //          info("evaluation failure: " + e + s" for $v1 and $v2")
//            case _                                   =>
//          }
//        }
//      }
//
//      results.size shouldBe >(0)
////      results should have size (56)
//
////      info(s"result with more than 6 results: " + {
////        val (k, v) = results.groupBy(_._1).find(_._2.size > 1).get
////        val v2 = v.map({ case (k, v) => v })
////        k + "\n" + v2
////      })
//
//      val extraction = new ExamplesExtraction(sctx, sctx.program)
//      
////      val resultsSmallest = results map {
////        case (k, v) =>
////          (k, v.toList.sortBy(ExprOps.formulaSize _).head)
////      }
//
//      val examples = results.map({
//        case (inEx, outEx) =>
//          ((in, inEx) :: Nil, (out, outEx))
//      }).toList
//
//      //      info("examples\n" + examples.mkString("\n"))
////      examples should have size (56)
//
//      //      test: attempt synthesis
//      // get fragments
//      val ((inIds, outId), transformedExamples) = ExamplesExtraction.transformMappings(examples).get
//      info(s"inIds $inIds")
//      info("transformed examples: " + transformedExamples.mkString("\n"))
//      val unorderedFragments = Fragmenter.constructFragments(transformedExamples, inIds)
//      info("unordered fragments: " + unorderedFragments.mkString("\n"))
//      
//      val unorederdFragmentsSet = unorderedFragments.toSet
//      info("unorederdFragmentsSet:\n" + unorederdFragmentsSet.mkString("\n"))
//      ???
//      
//      transformedExamples should have size unorderedFragments.size
//      val zipped = (transformedExamples zip unorderedFragments)
//      val groupped = zipped.groupBy(_._1._1)
//      val sorted =
//        for ( (input, list) <- groupped ) yield {
//          val fragments = list.map(_._2)
//          (input, fragments.sortBy(ExprOps.formulaSize _).head)
//        }
//
//      info("")
//      info("")
//      info("")
//      info("sorted:\n" + sorted.
//        map({ case (k, v) => k.head + "\n" + v }).mkString("\n******\n"))
////      info("unordered fragments set:\n" + (transformedExamples zip unorderedFragments).
////        map({ case (k, v) => k._1.head + "\n" + k._2 + "\n" + v }).mkString("\n******\n"))
////      info("unordered fragments set:\n" + unorderedFragments.toSet.mkString("\n\n\n"))
//
//    }
//
//  }

}