package lesynth

import scala.collection.mutable.{ Map => MutableMap }
import scala.collection.mutable.{ Set => MutableSet }
import scala.collection.mutable.{ LinkedList => MutableList }
import scala.util.matching.Regex
import scala.collection.mutable.PriorityQueue

import scala.tools.nsc.{ Settings => NSCSettings, MainGenericRunner }

import leon.{ Main => LeonMain, Reporter, DefaultReporter, SilentReporter, Settings, LeonContext }
import leon.solvers.{ Solver, TimeoutSolver }
import leon.solvers.z3.{ FairZ3Solver }
import leon.verification.AnalysisPhase
import leon.purescala.TypeTrees.{ TypeTree => LeonType, _ }
import leon.purescala.Trees.{ Variable => LeonVariable, _ }
import leon.purescala.Definitions.{ FunDef, VarDecl, Program, ObjectDef }
import leon.purescala.Common.{ Identifier, FreshIdentifier }
import leon.purescala.TreeOps
import leon.plugin.ExtractionPhase

import insynth.util.logging.HasLogger
import insynth.interfaces.Declaration
import insynth.InSynth
import insynth.reconstruction.codegen.CodeGenerator
import insynth.leon.loader.LeonLoader
import insynth.leon.LeonDeclaration
import insynth.leon.ImmediateExpression
import insynth.engine.InitialEnvironmentBuilder
import insynth.leon.LeonQueryBuilder
import insynth.interfaces.{ Loader, Declaration, QueryBuilder }
import insynth.engine.{ Engine, InitialEnvironmentBuilder }
import insynth.engine.scheduler.WeightScheduler
import insynth.structures.ContainerNode
import insynth.util.TimeOut
import insynth.Config
import insynth.reconstruction.Reconstructor
import insynth.leon.TypeTransformer
import insynth.leon.HoleFinder
import insynth.leon.loader.HoleExtractor
import insynth.reconstruction.Output

class Synthesizer(
  fileName: String,
  // number of condition expressions to try before giving up on that branch expression
	numberOfBooleanSnippets: Int = 5,
	numberOfCounterExamplesToGenerate: Int = 5,
	leonTimeout: Int = 2, // seconds
	analyzeSynthesizedFunctionOnly: Boolean = false,
	showLeonOutput: Boolean = false,
	reporter: Reporter = new DefaultReporter,
  filterOutAlreadySeenBranchExpressions: Boolean = true,
  useStringSetForFilterOutAlreadySeenBranchExpressions: Boolean = true
) extends HasLogger {
  
  info("Synthesizer:")
  info("fileName: %s".format(fileName))
  info("numberOfBooleanSnippets: %d".format(numberOfBooleanSnippets))
  info("numberOfCounterExamplesToGenerate: %d".format(numberOfCounterExamplesToGenerate))
  info("leonTimeout: %d".format(leonTimeout))

  // some synthesis instance information
  private var program: Program = _
  private var hole: Hole = _
  private var holeFunDef: FunDef = _
  // initial declarations
  private var allDeclarations: List[Declaration] = _
  // objects used in the synthesis
  private var loader: LeonLoader = _
  private var inSynth: InSynth = _
  private var inSynthBoolean: InSynth = _
  private var refiner: Refiner = _
  private var solver: Solver = _
  private var ctx: LeonContext = _
  private var initialPrecondition: Expr = _
  private var variableRefinements: MutableMap[Identifier, MutableSet[ClassType]] = _
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
  
  def analyzeProgram = {

    val temp = System.currentTimeMillis
    Globals.allSolved = Some(true)

    val report = AnalysisPhase.run(ctx)(program)

    val time = System.currentTimeMillis - temp
    fine("Analysis took: " + time + ", from report: " + report.totalTime)
    
    // accumulate
    verTime += time
    
    report
  }

  // TODO return boolean (do not do unecessary analyze)
  def generateCounterexamples(program: Program, funDef: FunDef, number: Int): (Seq[Map[Identifier, Expr]], Expr) = {

    fine("generate counter examples with funDef.prec= " + funDef.precondition.getOrElse(BooleanLiteral(true)))
    val temp = System.currentTimeMillis
    
    // get current precondition
    var precondition = funDef.precondition.getOrElse(BooleanLiteral(true))
    // where we will accumulate counterexamples as sequence of maps
    var maps: Seq[Map[Identifier, Expr]] = Seq.empty

    // iterate specific number of times or until no further counterexample can be generated
    var changed = true
    var ind = 0
    while (ind < number && changed) {

      // analyze the program
      analyzeProgram

      // check if solver could solved this instance
      if (Globals.allSolved == Some(false) && !Globals.asMap.isEmpty) {

        fine("found coounterexample: " + Globals.asMap)
        // add current counterexample
        maps :+= Globals.asMap
        
        // prevent this counterexample to re-occur
        val precAddition = Not(And(Globals.asMap map { case (id, value) => Equals(LeonVariable(id), value) } toSeq))
        precondition = And(Seq(precondition, precAddition))
        // update precondition        
        funDef.precondition = Some(precondition)
      } else
        changed = false

      ind += 1
    }

    val temptime = System.currentTimeMillis - temp
    fine("Generation of counter-examples took: " + temptime)
    verTime += temptime
    
    // return found counterexamples and the formed precondition
    (maps, precondition)
  }

  def getCurrentBuilder = new InitialEnvironmentBuilder(allDeclarations)
  
  def synthesizeBranchExpressions =
    inSynth.getExpressions(getCurrentBuilder)

  def synthesizeBooleanExpressions = {
    if ( variableRefinedCondition ) {
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

  def initialize = {
    
    // TODO lose this - globals are bad
    Globals.allSolved = None
    
    // extract hole
    new HoleFinder(fileName).extract match {
      case Some((matchProgram, matchHole: Hole)) =>
        program = matchProgram
        hole = matchHole
      case None => throw new RuntimeException("Cannot find hole")
    }
    
    // context needed for verification
    import leon.Main._
    val reporter = 
      if (showLeonOutput) new DefaultReporter
      else new SilentReporter
      
    val args =
      if (analyzeSynthesizedFunctionOnly)
      	Array(fileName, "--timeout=" + leonTimeout, "--functions=" + holeFunDef.id.name)
    	else
    	  Array(fileName, "--timeout=" + leonTimeout)    	  
  	info("Leon context array: " + args.mkString(","))
    ctx = processOptions(reporter, args.toList)
    
    solver = //new FairZ3Solver(ctx)
      new TimeoutSolver(new FairZ3Solver(ctx), leonTimeout)

    // create new insynth object
    loader = new LeonLoader(program, hole, false)
    inSynth = new InSynth(loader, true)
    // save all declarations seen
    allDeclarations = inSynth.getCurrentBuilder.getAllDeclarations
    // make conditions synthesizer
    inSynthBoolean = new InSynth(allDeclarations, BooleanType, true)
    
    // funDef of the hole
    holeFunDef = loader.holeDef
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
    val directSubclassMap = loader.directSubclassesMap
    val variableDeclarations = loader.variableDeclarations
    // map from identifier into a set of possible subclasses
    variableRefinements = MutableMap.empty
    for (varDec <- variableDeclarations) {
      varDec match {
        case LeonDeclaration(_, _, typeOfVar: ClassType, ImmediateExpression(_, LeonVariable(id))) =>
          variableRefinements += (id -> MutableSet(directSubclassMap(typeOfVar).toList: _*))
        case _ =>
      }
    }

    // calculate cases that should not happen
    refiner = new Refiner(program, hole, holeFunDef)
    fine("Refiner initialized. Recursive call: " + refiner.recurentExpression)
    
  }

  def synthesize: Report = {
    reporter.info("Synthesis called on file: " + fileName)
    
    // get start time
    startTime = System.currentTimeMillis

    reporter.info("Initializing synthesizer: ")
	  reporter.info("numberOfBooleanSnippets: %d".format(numberOfBooleanSnippets))
	  reporter.info("numberOfCounterExamplesToGenerate: %d".format(numberOfCounterExamplesToGenerate))
	  reporter.info("leonTimeout: %d".format(leonTimeout))
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
        
    // iterate while the program is not valid
    import scala.util.control.Breaks._
    var iteration = 0
    breakable {
      while (keepGoing) {
        // next iteration
        iteration += 1
        reporter.info("####################################")
        reporter.info("######Iteration #" + iteration + " ###############")
        reporter.info("####################################")

        // just printing of expressions
        finest(
	        ((snippets zip Iterator.range(0, 200).toStream) map {
	          case ((snippet: Output, ind: Int)) => ind + ": snippet is " + snippet.getSnippet
          }).mkString("\n")
        )
        reporter.info("# precondition is: " + holeFunDef.precondition.getOrElse(BooleanLiteral(true)))
        reporter.info("# accumulatingPrecondition is: " + accumulatingPrecondition)
        reporter.info("# accumulatingExpression(Unit) is: " + accumulatingExpression(UnitLiteral))
        reporter.info("####################################")

        // found precondition?
        found = false
        // try to find it
        breakable {
          // go through all snippets
        	for (
            snippet <- snippetsIterator; val snippetTree = snippet.getSnippet;
            // filter if seen
            if ! (seenBranchExpressions contains snippetTree.toString)
          ) {
            finest("snippetTree is: " + snippetTree)

            // skip avoidable calls
            if (!refiner.isAvoidable(snippetTree)) {              
              if (tryToSynthesizeBranch(snippetTree)) {
                // will set found if correct body is found
                break
              }
            } else {
              fine("Refiner filtered this snippet: " + snippetTree)
            }
                        
            // note that we add snippet to already seen only if we cannot find a branch for it
            seenBranchExpressions += snippetTree.toString

          } // for (snippet <- snippets
        } // breakable { for (snippet <- snippets

        // if did not found for any of the branch expressions
        if (found) {
			    val endTime = System.currentTimeMillis
          reporter.info("We are done, in time: " + (endTime - startTime))
          return new FullReport(holeFunDef, (endTime - startTime))
        }

        // get new snippets if needed
        if ( variableRefinedBranch ) {
        	// get new snippets
        	snippets = synthesizeBranchExpressions
        	snippetsIterator = snippets.iterator
        	
          // reset flag
          variableRefinedBranch = false
        }
      }
    } //breakable { while (!keepGoing) {

    new EmptyReport
  }

  def tryToSynthesizeBranch(snippetTree: Expr): Boolean = {
    // replace hole in the body with the whole if-then-else structure, with current snippet tree
    val oldBody = holeFunDef.getBody
    val newBody = accumulatingExpression(snippetTree)
    holeFunDef.body = Some(newBody)
    
    // precondition
    val oldPrecondition = holeFunDef.precondition.getOrElse(BooleanLiteral(true))
    holeFunDef.precondition = Some(initialPrecondition)

    snippetTree.setType(hole.desiredType)
    //holeFunDef.getBody.setType(hole.desiredType)
    
    // TODO spare one analyzing step
    // analyze the program
    fine("analyzing program for funDef:" + holeFunDef)
    analyzeProgram

    // check if solver could solved this instance
    if (Globals.allSolved == Some(true)) {
      // mark the branch found
      found = true

      reporter.info("Wooooooow we have a winner!")
      reporter.info("************************************")
      reporter.info("*********And the winner is**********")
      reporter.info(accumulatingExpression(snippetTree).toString)
      reporter.info("************************************")

      return true
    }

    // store appropriate values here, will be update in a finally branch
    var bodyToRestore = oldBody
    var preconditionToRestore = Some(oldPrecondition)

    // because first initial test
    holeFunDef.precondition = preconditionToRestore

    // get counterexamples
    fine("Going to generating counterexamples: " + holeFunDef)
    val (maps, precondition) = generateCounterexamples(program, holeFunDef, numberOfCounterExamplesToGenerate)

    // this should not be possible
//    val keepGoing = Globals.allSolved match {
//      case Some(true) => false
//      case _ => true
//    }
//
//    // if no counterexamples and all are solved, we are done
//    if (maps.isEmpty && !keepGoing) {
//      // mark the branch found
//      found = true
//
//      println("Wooooooow we have a winner!")
//      println("************************************")
//      println("*********And the winner is**********")
//      println(accumulatingExpression(snippetTree))
//      println("************************************")
//
//      return true
//    }

    // will modify funDef body and precondition, restore it later
    try {
      if (!maps.isEmpty) {
        // proceed with synthesizing boolean expressions
        solver.setProgram(program)

        // reconstruct (only defined number of boolean expressions)
        val innerSnippets = synthesizeBooleanExpressions
        // just printing of expressions
        finest(
	        ((innerSnippets zip Iterator.range(0, 500).toStream) map {
	          case ((snippet: Output, ind: Int)) => ind + ": snippet is " + snippet.getSnippet
          }).mkString("\n")
        )

        for (innerSnippetTree <- innerSnippets map { _.getSnippet }) {
          fine("boolean snippet is: " + innerSnippetTree)

          val (innerFound, innerPrec) = tryToSynthesizeBooleanCondition(snippetTree, innerSnippetTree, precondition)

          // if precondition found
          if (innerFound) {
            reporter.info("Precondition " + innerPrec + " found for " + snippetTree)
            
	          innerPrec match {
	            case s @ Some(_) =>
	              // new precondition (restore in finally)
	              preconditionToRestore = s
	            case _ =>
	          }
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
      // set these to the FunDef
      holeFunDef.precondition = preconditionToRestore
      // restore old body (we accumulate expression)                                
      holeFunDef.body = Some(oldBody)
    }
  }

  def tryToSynthesizeBooleanCondition(snippetTree: Expr, innerSnippetTree: Expr, precondition: Expr): (Boolean, Option[Expr]) = {

    // new condition together with existing precondition
    val newCondition = And(Seq(accumulatingPrecondition, innerSnippetTree))

    // new expression should not be false
    val notFalseEquivalence = Not(newCondition)
    val notFalseSolveReturn = solver.solve(notFalseEquivalence)
    fine("solve for not false returned: " + notFalseSolveReturn)
    
    notFalseSolveReturn match {
      case Some(true) =>
        (false, None)
      case None =>
        (false, None)
      // nothing here
      // here, our expression is not contradictory, continue
      case Some(false) =>
        // check if synthesized boolean expression implies precondition (counterexamples)
        val implyExpression = Implies(newCondition, precondition)
        fine("implyExpression is: " + implyExpression)

        // check if synthesized condition implies counter-examples
        val solveReturn = solver.solve(implyExpression)
        fine("solve returned: " + solveReturn)

        solveReturn match {
          case Some(true) =>
            // if expression implies counterexamples add it to the precondition and try to validate program
            holeFunDef.precondition = Some(newCondition)
            // do analysis
            analyzeProgram
            // program is valid, we have a branch
            if (Globals.allSolved == Some(true)) {
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

              // set to set new precondition
              val preconditionToRestore = Some(accumulatingPrecondition)

              // check for refinements
              checkRefinements(innerSnippetTree) match {
                case Some(refinementPair @ (id, classType)) =>
                  fine("And now we have refinement type: " + refinementPair)
                  fine("variableRefinements(id) before" + variableRefinements(id))
                  variableRefinements(id) -= loader.classMap(classType.id)
                  fine("variableRefinements(id) after" + variableRefinements(id))

                  // if we have a single subclass possible to refine
                  if (variableRefinements(id).size == 1) {
                    reporter.info("We do variable refinement for " + id)

                    val newType = variableRefinements(id).head
                    fine("new type is: " + newType)

                    // update declarations
                    allDeclarations =
                      for (dec <- allDeclarations)
                        yield dec match {
                        case LeonDeclaration(inSynthType, _, decClassType, imex @ ImmediateExpression(_, LeonVariable(`id`))) =>
                          LeonDeclaration(
                            imex, TypeTransformer(newType), newType)
                        case _ =>
                          dec
                      }
                    
                    // the reason for two flags is for easier management of re-syntheses only if needed 
                    variableRefinedBranch = true
                    variableRefinedCondition = true

                  } else
                    fine("we cannot do variable refinement :(")
                case _ =>
              }
                  
              // found a boolean snippet, break
              (true, preconditionToRestore)
            } else {
              // reset funDef and continue with next boolean snippet
              val preconditionToRestore = Some(accumulatingPrecondition)
              (false, preconditionToRestore)
            }

          case _ =>
            
            fine("solver filtered out the precondition (does not imply counterexamples)")
            (false, None)
        } //solveReturn match { (for implying counterexamples)
    } // notFalseSolveReturn match {
  }

  // inspect the expression if some refinements can be done
  def checkRefinements(expr: Expr) = expr match {
    case CaseClassInstanceOf(classDef, LeonVariable(id)) =>
      Some((id, classDef))
    case _ =>
      None
  }
  
}