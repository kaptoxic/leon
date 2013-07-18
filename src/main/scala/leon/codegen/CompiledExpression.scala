/* Copyright 2009-2013 EPFL, Lausanne */

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

import leon.codegen.runtime.RuntimeMonitor

class CompiledExpression(unit: CompilationUnit, cf: ClassFile, expression : Expr, argsDecl: Seq[Identifier]) {
  private val defParams = CodeGenEvalParams.default

  private lazy val cl = unit.loader.loadClass(cf.className)
  private lazy val meth = cl.getMethods()(0)

  private val exprType = expression.getType

  def argsToJVM(args: Seq[Expr]): Seq[AnyRef] = {
    args.map(unit.valueToJVM)
  }

  def evalToJVM(args: Seq[AnyRef], params : CodeGenEvalParams = defParams): AnyRef = {
    assert(args.size == argsDecl.size)

    val monitor : RuntimeMonitor = if(params == defParams) {
      // The generated code is instrumented to check for null, and to act as the default
      // configuration dictates in these cases.
      // This is slightly faster than reading the object each time just to ignore it.
      null
    } else {
      new RuntimeMonitor(
        params.checkContracts,
        params.maxFunctionInvocations
      )
    }

    if (args.isEmpty) {
      meth.invoke(null, Seq(monitor).toArray : _*)
    } else {
      meth.invoke(null, (monitor +: args.map(unit.valueToJVM)).toArray : _*)
    }
  }

  // This may throw an exception. We unwrap it if needed.
  // We also need to reattach a type in some cases (sets, maps).
  def evalFromJVM(args: Seq[AnyRef], params : CodeGenEvalParams = defParams) : Expr = {
    try {
      val result = unit.jvmToValue(evalToJVM(args, params))
      if(!result.isTyped) {
        result.setType(exprType)
      }
      result
    } catch {
      case ite : InvocationTargetException => throw ite.getCause()
    }
  }

  def eval(args: Seq[Expr]) : Expr = {
    try {
      evalFromJVM(argsToJVM(args))
    } catch {
      case ite : InvocationTargetException => throw ite.getCause()
    }
  }
} 
