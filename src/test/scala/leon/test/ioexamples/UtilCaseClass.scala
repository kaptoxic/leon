package leon.test.ioexamples

import leon._
import purescala._
import Expressions._
import Definitions._
import Types._
import Common._
import Constructors._

import org.scalatest.Assertions._

object UtilCaseClass {
  
  def caseClassDef(name: String)(implicit pgm: Program): CaseClassDef = {
    pgm.lookupAll(name).collect {
      case ccd: CaseClassDef => ccd
    }.headOption.getOrElse {
      fail(s"Failed to lookup case class '$name' in program")
    }
  } 
  
  def cc(name: String)(args: Expr*)(implicit pgm: Program): Expr = {
    val cct = caseClassDef(name).typed(Seq())
    CaseClass(cct, args.toSeq)
  }
  
  def caseClassField(caseClass: Expr, selector: String)(implicit pgm: Program): Expr = {
    caseClass.getType match {
      case cct: CaseClassType =>
        val field = cct.fields.find(_.id.name == selector).get.id
        CaseClassSelector(cct, caseClass, field)
      case at: AbstractClassType =>
        val cct = at.classDef.knownChildren.find(_.fields.map(_.id.name) contains selector).get.
          asInstanceOf[CaseClassDef].typed
        val field = cct.fields.find(_.id.name == selector).get.id
        CaseClassSelector(cct, caseClass, field)
      case t =>
        fail(s"Failed to find appropriate case class type, type is $t, for $caseClass")
    }
  }
  
}