package leon.test
package ioexamples

import leon.{ purescala => lscala }
import lscala.{ ScalaPrinter => PureScalaPrinter, _ }
import lscala.Extractors._
import lscala.Common._
import lscala.Expressions._
import lscala.Types._
import lscala.Definitions._
import PrinterHelpers._
import leon.xlang.Expressions._

import leon.test.condabd.util._

import org.scalatest._
import org.scalatest.Matchers._

/** This pretty-printer only print valid scala syntax */
class CustomPrinter(opts: PrinterOptions,
                   opgm: Option[Program],
                   sb: StringBuffer = new StringBuffer) extends PureScalaPrinter(opts, opgm, sb) {
  
  override def pp(tree: Tree)(implicit ctx: PrinterContext): Unit = {
    tree match {
      case CaseClass(consClass, args) if
        consClass.classDef.id.name == "Cons" && args.size == 2 =>
          
        if (ctx.parents.find({
          case CaseClass(consClass, _) if consClass.classDef.id.name == "Cons" => true
        }).isEmpty) {
          p"[${nary(args, "")}]"
        } else {
          p", ${nary(args, "")}"
        }

      case CaseClass(nilClass, args) if
        nilClass.classDef.id.name == "Nil" && args.size == 0 =>
          p""
      case _ =>
        super.pp(tree)
    }
  }
  
  override protected def requiresParentheses(ex: Tree, within: Option[Tree]): Boolean = {
    (ex, within) match {
      case (_, _) => super.requiresParentheses(ex, within)
    }
  }
  
  
  override def requiresBraces(ex: Tree, within: Option[Tree]) = (ex, within) match {
    case (_, _) => super.requiresBraces(ex, within)
  }

}

object CustomPrinter extends PrettyPrinterFactory {
  def create(opts: PrinterOptions, opgm: Option[Program]) = new CustomPrinter(opts, opgm)
}

class CustomPrinterTest extends FunSpec with Matchers with Inside {

  import Scaffold._
  import Constructors._

  val ioExamplesTestcaseDir = "testcases/synthesis/io-examples/"

  describe("correct printing") {

    it("print ExamplesAsSpecifications testcase") {

      val problems = forFile(ioExamplesTestcaseDir + "ExamplesAsSpecifications.scala").toList
      
      val (sctx, funDef, problem) = problems.head
      val program = sctx.program

      val consClass = program.caseClassDef("Cons").typed
      val nilClass = program.caseClassDef("Nil").typed
      val nilExp = CaseClass(nilClass, Nil): Expr
      
      CustomPrinter(CaseClass(consClass, IntLiteral(0) :: nilExp :: Nil)) shouldBe "[0]"
    }

  }

}
