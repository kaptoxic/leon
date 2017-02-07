package leon.test
package ioexamples
package backwards

import leon._
import purescala._
import Expressions._
import Definitions._
import Types._
import leon.solvers.z3._
import leon.solvers.Solver
import leon.synthesis._
import leon.synthesis.utils._
import leon.synthesis.ioexamples._
import leon.synthesis.ioexamples.backwards._

import leon.test.condabd.util._

import org.scalatest._
import org.scalatest.Matchers._

import java.io.{ BufferedWriter, FileWriter, File }

class TermSynthesizerTest extends FunSpec with Inside {

  import Scaffold._

  val ioExamplesTestcaseDir = "testcases/synthesis/io-examples/"

  describe("synthesizing expressions driven by type") {

    ignore("in an initial environment") {

    }

    describe("in the environment of the spec example") {

      val parses = forFile(ioExamplesTestcaseDir + "ExamplesAsSpecifications.scala")

      describe("expected trees should be synthesized") {

        it("should synthesize input variables") {

          val (sctx, _, p) = parses.
            find(_._2.id.name == "identity").get
          
          val termSynthesizer = new TermSynthesizer(sctx, p)
          
          val res = termSynthesizer.apply(Int32Type :: Nil)
          res should not be ('empty)
          
          val in = p.as.head
          res should contain (in.toVariable)

        }

        it("should synthesize more complex expressions") {
         
          val (sctx, _, p) = parses.
            find(_._2.id.name == "tail").get
          
          sctx.settings.functionsToIgnore should be ('empty)
          val termSynthesizer = new TermSynthesizer(sctx, p) 
          
          val listClass = sctx.program.definedClasses.find(_.id.name == "List").get.typed
          
          val res = termSynthesizer.apply(listClass :: Nil)
          res should not be ('empty)
          
          val in = p.as.head.toVariable
          res should contain (in)
          
          val tailFun = sctx.program.definedFunctions.find(_.id.name == "nil").get
          val recCall =
            FunctionInvocation(tailFun.typed, Seq(in))
               
          res should contain (recCall)

        }

      }
      
    }
    
    describe("simple examples for testing recursive calls") {
      
      
      it("should synthesize recursive calls") {
  
        for (
          (oldSctx, f, p) <- forProgram(
            """
      	    import leon.lang.{ Map => _, _ }
            import leon.lang.synthesis._
            
            object Test {
            
              sealed abstract class List
              case class Cons(head: Int, tail: List) extends List
              case class Nil() extends List
              
              def lst() = Cons(0, Nil())
            
              def rec(in: List) = in match {
                case Nil() => Nil()
                case Cons(h, t) => choose {
                  (out : List) => true
                }
              }

            }
    	    """)
        ) {
          val sctx = new SynthesisContext(
              context = oldSctx.copy(
                options = oldSctx.options ++ (LeonOption(SynthesisPhase.optIntroduceRecCalls)(false) :: Nil)
              ),
              settings = oldSctx.settings,
              functionContext = oldSctx.functionContext,
              program = oldSctx.program
            )
          val lst = sctx.program.definedFunctions.find(_.id.name == "lst").get.typed
          // this allows us to produce rec. calls
          sctx.findOptionOrDefault(SynthesisPhase.optIntroduceRecCalls) should be (false)
          
          val listClass = sctx.program.definedClasses.find(_.id.name == "List").get.typed
          val nilClass = sctx.program.caseClassDef("Nil").typed
          
          import leon.synthesis.grammars
          import leon.grammars.Tags
          import leon.grammars.Label
          import leon.grammars.aspects.Tagged
          import leon.purescala.Constructors.tupleTypeWrap
          val grammar = grammars.default(sctx, p)
          grammar.printProductions(println(_))(sctx)
          val targetType = Label(tupleTypeWrap(listClass :: Nil)).withAspect(Tagged(Tags.Top, 0, None))
          val productions =
          leon.grammars.SafeRecursiveCalls(sctx.program, p.ws, p.pc).
            getProductions(targetType)(sctx)
          productions should not be ('empty)
          
          val termSynthesizer = new TermSynthesizer(sctx, p) 
          
          val res = termSynthesizer.apply(listClass :: Nil)
          res should not be ('empty)
          
          val in = p.as.head.toVariable
          res should contain (in)
          
          val lstFun = sctx.program.definedFunctions.find(_.id.name == "lst").get.typed
          val recFun = sctx.program.definedFunctions.find(_.id.name == "rec").get.typed

          val tVal = p.pc.boundIds.find(_.name == "t").get.toVariable
          val recCall =
            FunctionInvocation(recFun, Seq(tVal))
               
          res should contain (recCall)
        }
      }
    }
    

  }

}
