package lesynth

import scala.collection.mutable.{ Map => MutableMap }
import scala.collection.mutable.{ Set => MutableSet }
import scala.collection.mutable.PriorityQueue

import leon.{ Reporter, DefaultReporter, SilentReporter, LeonContext }
import leon.purescala.TypeTrees.{ TypeTree => LeonType, _ }
import leon.purescala.Trees.{ Variable => LeonVariable, _ }
import leon.purescala.Definitions.{ FunDef, Program }
import leon.purescala.Common.{ Identifier, FreshIdentifier }
import leon.purescala.TreeOps
import leon.synthesis.Problem
import leon.Main.processOptions
import leon.purescala.TypeTrees._

import insynth.util.logging.HasLogger
import insynth.interfaces.Declaration
import insynth.InSynth
import insynth.leon.loader.LeonLoader
import insynth.leon.LeonDeclaration
import insynth.leon.ImmediateExpression
import insynth.engine.InitialEnvironmentBuilder
import insynth.interfaces.Declaration
import insynth.engine.InitialEnvironmentBuilder
import insynth.leon.TypeTransformer
import insynth.reconstruction.Output


import scala.collection.mutable.{ Map => MutableMap, LinkedList => MutableList, Set => MutableSet }
import scala.util.control.Breaks.break
import scala.util.control.Breaks.breakable

import lesynth.examples._
import lesynth.ranker._

case class SynthesizerForRuleExamples(
  // some synthesis instance information
  val program: Program,
  val desiredType: LeonType,
  val holeFunDef: FunDef,
  val problem: Problem,
  val freshResVar: LeonVariable,
  // number of condition expressions to try before giving up on that branch expression
  numberOfBooleanSnippets: Int = 5,
  reporter: Reporter = new DefaultReporter,
  //examples: List[Map[Identifier, Expr]] = Nil,
  // we need the holeDef to know the right ids
  introduceExamples: ((Seq[Identifier], LeonLoader) => List[Example]) = { (_, _) => Nil },
  filterOutAlreadySeenBranchExpressions: Boolean = true,
  useStringSetForFilterOutAlreadySeenBranchExpressions: Boolean = true,
  numberOfTestsInIteration: Int = 50,
  numberOfCheckInIteration: Int = 5,
  generateInputExamples: Boolean = true
  ) extends HasLogger {

  val fileName: String = "noFileName"

  info("Synthesizer:")
  info("fileName: %s".format(fileName))
  info("numberOfBooleanSnippets: %d".format(numberOfBooleanSnippets))
  info("holeFunDef: %s".format(holeFunDef))
  info("problem: %s".format(problem.toString))

  private var hole: Hole = _
  // initial declarations
  private var allDeclarations: List[Declaration] = _
  // objects used in the synthesis
  private var loader: LeonLoader = _
  private var inSynth: InSynth = _
  private var inSynthBoolean: InSynth = _
  private var refiner: Refiner = _
  //private var solver: Solver = _
  private var ctx: LeonContext = _
  private var initialPrecondition: Expr = _

  private var variableRefiner: VariableRefiner = _
  // can be used to unnecessary syntheses
  private var variableRefinedBranch = false
  private var variableRefinedCondition = true // assure initial synthesis
  private var booleanExpressionsSaved: Stream[Output] = _

  private var seenBranchExpressions: Set[String] = Set.empty

  // flag denoting if a correct body has been synthesized
  private var found = false

  // accumulate precondition for the remaining branch to synthesize 
  private var accumulatingPrecondition: Expr = _
  // accumulate the final expression of the hole
  private var accumulatingExpression: Expr => Expr = _
  //private var accumulatingExpressionMatch: Expr => Expr = _

  // time
  var startTime: Long = _
  var verTime: Long = 0
  var synTime: Long = 0

  // filtering/ranking with examples support
  var exampleRunner: ExampleRunner = _
  var counterExamples = Seq[Example]()

  def getCurrentBuilder = new InitialEnvironmentBuilder(allDeclarations)

  def synthesizeBranchExpressions =
    inSynth.getExpressions(getCurrentBuilder)

  def synthesizeBooleanExpressions = {
    if (variableRefinedCondition) {
      // store for later fetch (will memoize values)
      booleanExpressionsSaved =
        inSynthBoolean.getExpressions(getCurrentBuilder) take numberOfBooleanSnippets
      // reset flag
      variableRefinedCondition = false
    }

    booleanExpressionsSaved
  }

  def interactivePause = {
    System.out.println("Press Any Key To Continue...");
    new java.util.Scanner(System.in).nextLine();
  }

  type QueueValue = (Expr, List[Example], List[Example])
  def getNewExampleQueue = PriorityQueue[(QueueValue, Int)]()(
    new Ordering[(QueueValue, Int)] {
      def compare(pair1: (QueueValue, Int), pair2: (QueueValue, Int)) =
        pair1._2.compare(pair2._2)
    })

  def initialize = {
    hole = Hole(desiredType)

    // TODO lose this - globals are bad
    Globals.allSolved = None

    // create new insynth object
    loader = new LeonLoader(program, hole, problem.as, false)
    inSynth = new InSynth(loader, true)
    // save all declarations seen
    allDeclarations = inSynth.getCurrentBuilder.getAllDeclarations
    // make conditions synthesizer
    inSynthBoolean = new InSynth(allDeclarations, BooleanType, true)

    // funDef of the hole
    fine("postcondition is: " + holeFunDef.getPostcondition)

    // accumulate precondition for the remaining branch to synthesize 
    accumulatingPrecondition = holeFunDef.precondition.getOrElse(BooleanLiteral(true))
    // save initial precondition
    initialPrecondition = accumulatingPrecondition
    // accumulate the final expression of the hole
    accumulatingExpression = (finalExp: Expr) => finalExp
    //accumulatingExpressionMatch = accumulatingExpression

    // each variable of super type can actually have a subtype
    // get sine declaration maps to be able to refine them  
    variableRefiner = new VariableRefiner(loader.directSubclassesMap,
      loader.variableDeclarations, loader.classMap, reporter)

    // calculate cases that should not happen
    refiner = new Refiner(program, hole, holeFunDef)
    fine("Refiner initialized. Recursive call: " + refiner.recurentExpression)

    exampleRunner = new ExampleRunner(program)
    if (generateInputExamples)
	  counterExamples ++= introduceExamples(holeFunDef.args.map(_.id), loader)
    // add examples from passes      
    counterExamples ++=
	  ExamplesExtraction.extract(problem.phi, holeFunDef.args.map(_.id).toSet)

    fine("Introduced examples: " + counterExamples.mkString(", "))
  }

  def countPassedExamples(snippet: Expr) = {
    val oldPreconditionSaved = holeFunDef.precondition
    val oldBodySaved = holeFunDef.body

    // restore initial precondition
    holeFunDef.precondition = Some(initialPrecondition)

    // get the whole body (if else...)
    val accumulatedExpression = accumulatingExpression(snippet)
    // set appropriate body to the function for the correct evaluation
    holeFunDef.body = Some(accumulatedExpression)

    fine("going to count passed for: " + holeFunDef)
    fine("going to count passed for body: " + accumulatedExpression)

    val results = exampleRunner.countPassedForBody(accumulatedExpression, holeFunDef.getPostcondition, counterExamples)
    //    if (snippet.toString == "Cons(l1.head, concat(l1.tail, l2))")
    //      interactivePause

    holeFunDef.precondition = oldPreconditionSaved
    holeFunDef.body = oldBodySaved

    results
  }

  def evaluateCandidate(snippet: Expr, example: Example) = {
    val mapping = example.getMapping
    
    val oldPreconditionSaved = holeFunDef.precondition
    val oldBodySaved = holeFunDef.body

    // restore initial precondition
    holeFunDef.precondition = Some(initialPrecondition)

    // get the whole body (if else...)
    val accumulatedExpression = accumulatingExpression(snippet)
    // set appropriate body to the function for the correct evaluation
    holeFunDef.body = Some(accumulatedExpression)

    val expressionToCheck = example.getExpression(accumulatedExpression, holeFunDef.getPostcondition)

    fine("going to evaluate candidate for: " + holeFunDef)
    fine("going to evaluate candidate for: " + expressionToCheck)

    val results = exampleRunner.evaluate(expressionToCheck, mapping)
    //    if (snippet.toString == "Cons(l1.head, concat(l1.tail, l2))")
    //      interactivePause

    holeFunDef.precondition = oldPreconditionSaved
    holeFunDef.body = oldBodySaved

    results
  }

  def synthesize: Report = {
    reporter.info("Synthesis called on file: " + fileName)

    // get start time
    startTime = System.currentTimeMillis

    reporter.info("Initializing synthesizer: ")
    reporter.info("numberOfBooleanSnippets: %d".format(numberOfBooleanSnippets))
    initialize
    reporter.info("Synthesizer initialized")

    // keeps status of validity
    var keepGoing = Globals.allSolved match {
      case Some(true) => false
      case _ => true
    }

    // initial snippets (will update in the loop)
    var snippets = synthesizeBranchExpressions
    var snippetsIterator = snippets.iterator

    // ordering of expressions according to passed examples
    var pq = getNewExampleQueue

    // iterate while the program is not valid
    import scala.util.control.Breaks._
    var iteration = 0
    var noBranchFoundIteration = 0
    breakable {
      while (keepGoing) {
        // next iteration
        iteration += 1
        noBranchFoundIteration += 1
        reporter.info("####################################")
        reporter.info("######Iteration #" + iteration + " ###############")
        reporter.info("####################################")
        reporter.info("# precondition is: " + holeFunDef.precondition.getOrElse(BooleanLiteral(true)))
        reporter.info("# accumulatingPrecondition is: " + accumulatingPrecondition)
        reporter.info("# accumulatingExpression(Unit) is: " + accumulatingExpression(UnitLiteral))
        reporter.info("####################################")

        var numberOfTested = 0

        // just printing of expressions and pass counts        
        finer({
          val (it1, it2) = snippetsIterator.duplicate // we are dealing with iterators, need to duplicate
          val logString = ((it1 zip Iterator.range(0, numberOfTestsInIteration)) map {
            case ((snippet: Output, ind: Int)) => ind + ": snippet is " + snippet.getSnippet +
              " pass count is " + countPassedExamples(snippet.getSnippet)._1.size
          }).mkString("\n")
          snippetsIterator = it2
          throw new RuntimeException
          logString
        })
        //interactivePause

        reporter.info("Going into a enumeration/testing phase.")
        finer("evaluating examples: " + counterExamples.mkString("\n"))

        // found precondition?
        found = false
        // try to find it
        breakable {
          while (true) {
            val batchSize = numberOfTestsInIteration * (1 << noBranchFoundIteration)

            reporter.info("numberOfTested: " + numberOfTested)
            // ranking of candidates        
            val candidates = {
              val (it1, it2) = snippetsIterator.duplicate
              snippetsIterator = it2.drop(batchSize)
              it1.take(batchSize).
                map(_.getSnippet).filterNot(
                  snip => {
                    (seenBranchExpressions contains snip.toString) || refiner.isAvoidable(snip, problem.as)
                  }).toSeq
            }
            info("got candidates of size: " + candidates.size)
            //interactivePause

            if (candidates.size > 0) {
              val evaluation = Evaluation(counterExamples, this.evaluateCandidate _, candidates, exampleRunner)
              val ranker = new Ranker(candidates.size, evaluation, false)

              val maxCandidate = candidates(ranker.getMax)

              numberOfTested += batchSize

              if (candidates.exists(_.toString contains
                //		            "append(append(quickSort(smaller(list.head, list.tail)), Cons(list.head, equals(list.head, list.tail))), quickSort(greater(list.head, list.tail)))"
                "append(append(quickSort(smaller(list.head, list.tail)), Cons(list.head, equals(list.head, list.tail)))")) {
                println("maxCandidate is: " + maxCandidate)
                println(ranker.printTuples)
                println("AAA2")
                //			          println("Candidates: " + candidates.zipWithIndex.map({
                //		              case (cand, ind) => "[" + ind + "]" + cand.toString
                //			          }).mkString("\n"))
                //			          println("Examples: " + exampleRunner.counterExamples.zipWithIndex.map({
                //		              case (example, ind) => "[" + ind + "]" + example.toString
                //			          }).mkString("\n"))
                interactivePause
              }

              // if all remaining examples pass (with current expression as else branch) we are done
              val countPassedResults = countPassedExamples(maxCandidate)
              info("maxCandidate is: " + maxCandidate)
              fine("passed/failed: " + countPassedResults._1.size + "/" + countPassedResults._2.size)
              interactivePause
              if (countPassedResults._1.size == counterExamples.size) {
                found = true
                break
              }

              if (countPassedResults._1.size > 0 && tryToSynthesizeBranch((maxCandidate, countPassedResults._1, countPassedResults._2))) {
                noBranchFoundIteration = 0
                break
              }
              //		          interactivePause

              noBranchFoundIteration += 1
            }
          }
        }

        // if did not found for any of the branch expressions
        if (found) {
          val endTime = System.currentTimeMillis
          reporter.info("We are done, in time: " + (endTime - startTime))
          return new FullReport(holeFunDef, (endTime - startTime))
        }

        fine("filtering based on: " + accumulatingPrecondition)
        fine("counterexamples before filter: " + counterExamples.size)
        counterExamples = exampleRunner.filter(accumulatingPrecondition, counterExamples)
        fine("counterexamples after filter: " + counterExamples.size)
        fine("counterexamples after filter: " + counterExamples.mkString("\n"))

        if (!variableRefinedBranch) {
          // XXX very ad-hoc, change this with something smarter
          // this has to be done if stronger condition filters out examples but variable refinement is not done (example:
          // quick sort and first condition is_sorted)

          breakable {
            for (
              booleanOutput <- booleanExpressionsSaved;
              val booleanExpression = booleanOutput.getSnippet
            ) {
              val expressionToCheck = booleanExpression
              fine("expression: " + booleanExpression + " passed: " + exampleRunner.countPassed(expressionToCheck, counterExamples)._1.size)
              if (exampleRunner.countPassed(expressionToCheck, counterExamples)._1.size == counterExamples.size)
                expressionToCheck match {
                  case CaseClassInstanceOf(classDef, LeonVariable(id)) =>
                    allDeclarations = variableRefiner.updateDeclarations(id, loader.classMap(classDef.id), allDeclarations)
                    fine("declaration : " + id + " updated to " + classDef)

                    variableRefinedBranch = true
                    variableRefinedCondition = true
                    //					          interactivePause

                    break
                  case _ =>
                }
            }
          }
        }

        if (variableRefinedBranch) {
          fine("Variable refined, doing branch synthesis again")
          // get new snippets
          snippets = synthesizeBranchExpressions
          snippetsIterator = snippets.iterator

          // reset flag
          variableRefinedBranch = false
        }

        // reseting iterator needed because we may have some expressions that previously did not work
        snippetsIterator = snippets.iterator
        // reseting the queue
        pq = getNewExampleQueue
      }
    } //breakable { while (!keepGoing) {

    EmptyReport
  }

  def tryToSynthesizeBranch(queueTuple: QueueValue): Boolean = {
    val (snippetTree, passed, failed) = queueTuple

    //    // replace hole in the body with the whole if-then-else structure, with current snippet tree
    //    val oldBody = holeFunDef.getBody
    //    val newBody = accumulatingExpression(snippetTree)
    //    holeFunDef.body = Some(newBody)
    //
    //    // precondition
    //    val oldPrecondition = holeFunDef.precondition.getOrElse(BooleanLiteral(true))
    //    holeFunDef.precondition = Some(initialPrecondition)
    //
    //    snippetTree.setType(hole.desiredType)
    //holeFunDef.getBody.setType(hole.desiredType)

    // no analysis is performed

    // store appropriate values here, will be update in a finally branch
    //    var bodyToRestore = oldBody
    //    var preconditionToRestore = Some(oldPrecondition)
    //
    //    // because first initial test
    //    holeFunDef.precondition = preconditionToRestore

    // will modify funDef body and precondition, restore it later
    try {
      { //if (!maps.isEmpty) {
        // proceed with synthesizing boolean expressions
        // reconstruct (only defined number of boolean expressions)
        val innerSnippets = synthesizeBooleanExpressions
        // just printing of expressions
        fine(
          ((innerSnippets zip Iterator.range(0, numberOfBooleanSnippets).toStream) map {
            case ((snippet: Output, ind: Int)) => ind + ": snippet is " + snippet.getSnippet
          }).mkString("\n"))

        for (
          innerSnippetTree <- innerSnippets map { _.getSnippet };
          if (
            {
              val flag = !refiner.isAvoidable(innerSnippetTree, problem.as)
              if (!flag) fine("Refiner filtered this snippet: " + innerSnippetTree)
              flag
            })
        ) {
          fine("boolean snippet is: " + innerSnippetTree)

          val innerFound = tryToSynthesizeBooleanCondition(queueTuple, innerSnippetTree)

          // if precondition found
          if (innerFound) {
            reporter.info("Precondition " + " found for " + snippetTree)

            return true
          }

        } // iterating over all boolean solutions

        reporter.info("No precondition found for branch expression: " + snippetTree)
        return false

      } // if ( !maps.isEmpty ) {
      // no counter examples, we just say that we cannot do anything
      false
    } // try
    finally {
      //      // set these to the FunDef
      //      holeFunDef.precondition = preconditionToRestore
      //      // restore old body (we accumulate expression)                                
      //      holeFunDef.body = Some(oldBody)
    }
  }

  def tryToSynthesizeBooleanCondition(queueTuple: QueueValue, innerSnippetTree: Expr) = {
    val (snippetTree, passed, failed) = queueTuple

    // new condition together with existing precondition
    val newCondition = And(Seq(accumulatingPrecondition, innerSnippetTree))

    //        // if expression implies counterexamples add it to the precondition and try to validate program
    //        holeFunDef.precondition = Some(newCondition)

    val enabledPassed = exampleRunner.countPassed(newCondition, passed)
    val enabledFailed = exampleRunner.countPassed(newCondition, failed)
    val conditionAcceptable =
      enabledFailed._1.size == 0 &&
        enabledPassed._1.size == passed.size
    fine("passed but disabled: " + enabledPassed._2.mkString(", "))
    fine("disabled failed/total: " + enabledFailed._1.size + "/" + failed.size +
      " enabled passed: " + enabledPassed._1.size + "/" + passed.size)
    //    interactivePause

    // program is valid, we have a branch
    if (conditionAcceptable) {
      // we found a branch
      reporter.info("We found a branch, for expression %s, with condition %s.".format(snippetTree, innerSnippetTree))

      // update accumulating expression
      val oldAccumulatingExpression = accumulatingExpression
      val newAccumulatingExpression =
        (finalExpr: Expr) =>
          oldAccumulatingExpression({
            val innerIf = IfExpr(innerSnippetTree, snippetTree, finalExpr)
            innerIf.setType(snippetTree.getType)
            innerIf
          })

      accumulatingExpression = newAccumulatingExpression

      // update accumulating precondition
      fine("updating accumulatingPrecondition")
      accumulatingPrecondition = And(Seq(accumulatingPrecondition, Not(innerSnippetTree)))
      fine("updating hole fun precondition and body (to be hole)")

      val variableRefinementResult = variableRefiner.checkRefinements(innerSnippetTree, allDeclarations)
      if (variableRefinementResult._1) {
        allDeclarations = variableRefinementResult._2

        // the reason for two flags is for easier management of re-syntheses only if needed 
        variableRefinedBranch = true
        variableRefinedCondition = true
      }

      // found a boolean snippet, break
      true
    } else {
      // reset funDef and continue with next boolean snippet
      val preconditionToRestore = Some(accumulatingPrecondition)
      false
    }

  }

}