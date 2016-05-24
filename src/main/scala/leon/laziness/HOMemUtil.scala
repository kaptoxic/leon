package leon
package laziness

import purescala.ScalaPrinter
import purescala.Common._
import purescala.Definitions._
import purescala.Expressions._
import purescala.ExprOps._
import purescala.DefOps._
import purescala.Types._
import java.io.File
import java.io.FileWriter
import java.io.BufferedWriter
import scala.util.matching.Regex
import utils.FileOutputPhase
import invariant.util.PredicateUtil._
import invariant.util.Util._

object HOMemUtil {

  def hasMemAnnotation(fd: FunDef) = {
    fd.flags.contains(Annotation("memoize", Seq()))
  }
  
  def isMemoized(fd: FunDef) = {
    fd.id.name.contains("-mem")
  }

  def prettyPrintProgramToFile(p: Program, ctx: LeonContext, suffix: String, uniqueIds: Boolean = false) {
    val optOutputDirectory = FileOutputPhase.optOutputDirectory
    val outputFolder = ctx.findOptionOrDefault(optOutputDirectory)
    try {
      new File(outputFolder).mkdir()
    } catch {
      case _: java.io.IOException =>
        ctx.reporter.fatalError("Could not create directory " + outputFolder)
    }

    for (u <- p.units if u.isMainUnit) {
      val outputFile = s"$outputFolder${File.separator}${u.id.toString}$suffix.scala"
      try {
        val out = new BufferedWriter(new FileWriter(outputFile))
        val plainText = ScalaPrinter.apply(u, purescala.PrinterOptions(printUniqueIds = uniqueIds))
        //println("Plain text: "+plainText)
        // remove '@' from the end of the identifier names
        val pat = new Regex("""(\w+)(@)""", "base", "at")
        val pgmText = try{ pat.replaceAllIn(plainText,
          m => {
            m.group("base")
//            + (if (!m.group("star").isEmpty()) "S" else "")
          })
        } catch {
          case _: IndexOutOfBoundsException => plainText
        }
        out.write(pgmText)
        out.close()
      } catch {
        case _: java.io.IOException => ctx.reporter.fatalError("Could not write on " + outputFile)
      }
    }
    ctx.reporter.info("Output written on " + outputFolder)
  }

  def isLazyInvocation(e: Expr)(implicit p: Program): Boolean = e match {
    case FunctionInvocation(TypedFunDef(fd, _), Seq(_)) =>
      fullName(fd)(p) == "leon.lazyeval.$"
    case _ =>
      false
  }

  def isEagerInvocation(e: Expr)(implicit p: Program): Boolean = e match {
    case FunctionInvocation(TypedFunDef(fd, _), Seq(_)) =>
      fullName(fd)(p) == "leon.lazyeval.eager"
    case _ =>
      false
  }

  def isInStateCall(e: Expr)(implicit p: Program): Boolean = e match {
    case FunctionInvocation(TypedFunDef(fd, _), Seq()) =>
      val fn = fullName(fd)(p)
      (fn == "leon.lazyeval.inState" || fn == "leon.mem.inState")
    case _ =>
      false
  }

  def isOutStateCall(e: Expr)(implicit p: Program): Boolean = e match {
    case FunctionInvocation(TypedFunDef(fd, _), Seq()) =>
      val fn = fullName(fd)(p)
      (fn == "leon.lazyeval.outState" || fn == "leon.mem.outState")
    case _ =>
      false
  }

  def cachedInvocation(e: Expr)(implicit p: Program): Boolean = e match {
    case FunctionInvocation(TypedFunDef(fd, _), Seq(_)) =>
      fullName(fd)(p) == "leon.mem.Fun.cached"
    case _ => false
  }

  def isWithStateCons(e: Expr)(implicit p: Program): Boolean = e match {
    case CaseClass(cct, Seq(_)) =>
      val fn = fullName(cct.classDef)(p)
      (fn == "leon.lazyeval.WithState" || fn == "leon.mem.memWithState")
    case _ => false
  }

  def isFunType(t: TypeTree)(implicit p: Program) = t match {
    case cct: CaseClassType => fullName(cct.classDef)(p) == "leon.mem.Fun"
    case _ => false
  }

  def isFunSetType(t: TypeTree)(implicit p: Program) = t match {
    case SetType(baseType) => isFunType(baseType)
    case _ => false
  }

  def isFunCons(e: Expr)(implicit p: Program): Boolean = e match {
    case CaseClass(cct, Seq(_)) =>
      fullName(cct.classDef)(p) == "leon.mem.Fun"
    case _ => false
  }

  def isFunMatch(e: Expr)(implicit p: Program): Boolean = e match {
   case FunctionInvocation(TypedFunDef(fd, _), _)  =>
      fullName(fd)(p) == "leon.higherorder.Fmatch.fmatch"
    case _ => false
  }

  def isIsFun(e: Expr)(implicit p: Program): Boolean = e match {
   case FunctionInvocation(TypedFunDef(fd, _), _)  =>
      fullName(fd)(p) == "leon.higherorder.Is.is"
    case _ => false
  }

  /**
   * There are many overloads of withState functions with different number
   * of arguments. All of them should pass this check.
   */
  def isWithStateFun(e: Expr)(implicit p: Program): Boolean = e match {
    case FunctionInvocation(TypedFunDef(fd, _), _) =>
      val fn = fullName(fd)(p)
      (fn == "leon.lazyeval.WithState.withState" ||
          fn == "leon.mem.memWithState.withState")
    case _ => false
  }

  def isValueInvocation(e: Expr)(implicit p: Program): Boolean = e match {
    case FunctionInvocation(TypedFunDef(fd, _), Seq(_)) =>
      fullName(fd)(p) == "leon.lazyeval.Lazy.value"
    case _ => false
  }

  def isStarInvocation(e: Expr)(implicit p: Program): Boolean = e match {
    case FunctionInvocation(TypedFunDef(fd, _), Seq(_)) =>
      fullName(fd)(p) == "leon.mem.Star.*"
    case _ => false
  }

  def isLazyType(tpe: TypeTree): Boolean = tpe match {
    case CaseClassType(CaseClassDef(cid, _, None, false), Seq(_)) =>
      cid.name == "Lazy"
    case _ => false
  }

  def capturedVars(l: Lambda) = l match {
    case Lambda(args, FunctionInvocation(_, allArgs)) if allArgs.forall(_.isInstanceOf[Variable]) =>
      val argvars = args.map(_.id).toSet
      (allArgs.map{ case Variable(id) => id }.filterNot(argvars.contains)).toList
    case _ =>
      throw new IllegalStateException("Lambda is not in expected form: "+l)
  }

  /**
   * Nested Closure types are not yet supported. TODO: handle them
   */
  /*def mapClosureType(tpe: TypeTree) = tpe match {
    case FunctionType =>
      Some(innerType)
    case _ => None
  }*/

  def opNameToCCName(name: String) = {
    name.capitalize + "@"
  }

  /**
   * Convert the first character to lower case
   * and remove the last two characters.
   */
  def ccNameToOpName(name: String) = {
    name.substring(0, 1).toLowerCase() +
      name.substring(1, name.length() - 2)
  }

  def typeNameToADTName(name: String) = {
    name + "@"
  }

  def adtNameToTypeName(name: String) = {
    name.substring(4)
  }

  def typeToFieldName(name: String) = {
    name.toLowerCase()
  }

  def closureConsName(typeName: String) = {
    "new@" + typeName
  }

  def isClosureCons(fd: FunDef) = {
    fd.id.name.startsWith("new@")
  }

  def evalFunctionName(absTypeName: String) = {
    "eval@" + absTypeName
  }

  def isEvalFunction(fd: FunDef) = {
    fd.id.name.startsWith("eval@")
  }

  def isStateParam(id: Identifier) = {
    id.name.startsWith("st@")
  }
  
  def isStateType(t: TypeTree) = {
     t match {
       case SetType(AbstractClassType(adef, _)) =>
         adef.root.id.name.startsWith("MemoFuns@")         
       case _ => false
     }
  }

  def isPlaceHolderTParam(tp: TypeParameter) = {
    tp.id.name.endsWith("@")
  }

  def freshenTypeArguments(tpe: TypeTree): TypeTree = {
    tpe match {
      case NAryType(targs, tcons) =>
        val ntargs = targs.map {
          case targ: TypeParameter => targ.freshen
          case targ                => targ
        }
        tcons(ntargs)
    }
  }

  /**
   * Returns the conjuncts of the  preconditions that contain
   * at least one argument (possibly in addition to captured variables)
   */
  def argPreconditions(l: Lambda): Expr ={
    l match {
      case Lambda(argDefs, FunctionInvocation(TypedFunDef(fd, _), _)) if fd.hasPrecondition =>
        val args = argDefs.map(_.id).toSet
        val pres = fd.precondition.get match {
          case And(conjs) => conjs
          case pre       => Seq(pre)
        }
        createAnd(pres.filter { p => !variablesOf(p).intersect(args).isEmpty })
      case _ => tru
    }
  }

  /**
   * @param st expression representing state at the point when the function is called.
   * Returns all preconditions that depend only on captured variables
   * First return value: state independent precondition,
   * Second return value: state dependent
   */
  def capturedPreconditions(l: Lambda): Expr = {
    l match {
      case Lambda(_, FunctionInvocation(TypedFunDef(fd, _), _)) if fd.hasPrecondition =>
        val capturedvars = capturedVars(l)
        val pres = fd.precondition.get match {
          case And(args) => args
          case pre       => Seq(pre)
        }
        createAnd(pres.filter { p => (variablesOf(p) -- capturedvars).isEmpty })
      case _ => tru
    }
  }
}
