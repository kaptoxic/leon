package leon
package codegen

import purescala.Common._
import purescala.Definitions._
import purescala.Trees._
import purescala.TypeTrees._

import cafebabe._
import cafebabe.AbstractByteCodes._
import cafebabe.ByteCodes._
import cafebabe.ClassFileTypes._
import cafebabe.Flags._

import java.lang.reflect.InvocationTargetException

import leon.codegen.runtime.Ticker

class CompiledExpression(unit: CompilationUnit, cf: ClassFile, expression : Expr, argsDecl: Seq[Identifier]) {
  private lazy val cl = unit.loader.loadClass(cf.className)
  private lazy val meth = cl.getMethods()(0)

  private val exprType = expression.getType

  protected[codegen] def evalToJVM(args: Seq[Expr], maxInvocations : Int = -1): AnyRef = {
    assert(args.size == argsDecl.size)

    val ticker : Ticker = if(maxInvocations < 0) null else (new Ticker(maxInvocations))

    if (args.isEmpty) {
      meth.invoke(null, Seq(ticker).toArray : _*)
    } else {
      meth.invoke(null, (ticker +: args.map(unit.valueToJVM)).toArray : _*)
    }
  }

  // This may throw an exception. We unwrap it if needed.
  // We also need to reattach a type in some cases (sets, maps).
  def eval(args: Seq[Expr], maxInvocations : Int = 1000) : Expr = {
    try {
      val result = unit.jvmToValue(evalToJVM(args))
      if(!result.isTyped) {
        result.setType(exprType)
      }
      result
    } catch {
      case ite : InvocationTargetException => throw ite.getCause()
    }
  }
} 
