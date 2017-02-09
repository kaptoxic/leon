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
import leon.utils.logging._

import org.scalatest._
import org.scalatest.Matchers._

/** This pretty-printer only print valid scala syntax */
class CustomPrinter(opts: PrinterOptions,
                   opgm: Option[Program],
                   sb: StringBuffer = new StringBuffer) extends
                   PureScalaPrinter(opts, opgm, sb) with HasLogger {
  
  override def pp(tree: Tree)(implicit ctx: PrinterContext): Unit = {
  
    // my goodness, really...
//    def nary(ls: Seq[Any], sep: String = ", ", init: String = "", closing: String = ""): Printable = {
//      val (i, c) = if(ls.isEmpty) ("", "") else (init, closing)
//      val strs = i +: List.fill(ls.size-1)(sep) :+ c
// 
//      new StringContext(strs: _*).p(ls: _*)
//    }

    tree match {
      case CaseClass(consClass, args) if
        consClass.classDef.id.name == "Cons" && args.size == 2 =>
          
        val h :: t = args
          
        finest(ctx.toString)
        if (ctx.parents.find({
          case CaseClass(consClass, _) if consClass.classDef.id.name == "Cons" => true
        }).isEmpty) {
          finer(s"expression is $tree and parents are ${ctx.parents}")
//          p"[${nary(args, "")}]"
          p"[$h$t]"
        } else {
//          p", ${nary(args, "")}"
          p",$h$t"
        }

      case CaseClass(nilClass, args) if
        nilClass.classDef.id.name == "Nil" && args.size == 0 =>
          p""
          
      case CaseClassSelector(ct, e@CaseClassSelector(ct2, e2, id2), id) if 
        ct.id.name == "Cons" && ct2.id.name == "Cons" =>
          p"${id.name.head}$e"
          
      case CaseClassSelector(ct, e, id) if 
        ct.id.name == "Cons" =>
          p"${id.name.head}($e)"
          
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
      
      val b0e = CaseClass(consClass, IntLiteral(0) :: nilExp :: Nil)
      val b01e = CaseClass(consClass, IntLiteral(1) :: b0e :: Nil)
      
      CustomPrinter(b0e) shouldBe "[0]"
      CustomPrinter(b01e) shouldBe "[1,0]"
      
      val sh = CaseClassSelector(consClass, problem.as.head.toVariable, consClass.fields.head.id)
      val sth =
        CaseClassSelector(consClass, sh, consClass.fields(1).id)
      CustomPrinter(sh) shouldBe "h(in)"
      CustomPrinter(sth) shouldBe "th(in)"
    }

  }

}
