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

// Be careful if you change these defaults. Passing `null` for a monitor
// to a compiled function should have the same effect (and it is hardcoded
// in `CodeGeneration.scala`).
case class CodeGenEvalParams(
  evaluateContracts : Boolean = false,  // Should we check pre- and post-conditions?
  maxFunctionInvocations : Int = -1     // Negative means unbounded.
)

class CompiledExpression(unit: CompilationUnit, cf: ClassFile, expression : Expr, argsDecl: Seq[Identifier]) {
  private val defaultParams = CodeGenEvalParams()

  private lazy val cl = unit.loader.loadClass(cf.className)
  private lazy val meth = cl.getMethods()(0)

  private val exprType = expression.getType

  protected[codegen] def evalToJVM(args: Seq[Expr], params : CodeGenEvalParams = defaultParams): AnyRef = {
    assert(args.size == argsDecl.size)

    val monitor : RuntimeMonitor = if(params == defaultParams) {
      null
    } else {
      new RuntimeMonitor(
        params.evaluateContracts,
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
  def eval(args: Seq[Expr], params : CodeGenEvalParams = defaultParams) : Expr = {
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
} 
