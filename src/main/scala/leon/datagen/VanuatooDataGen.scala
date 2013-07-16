/* Copyright 2009-2013 EPFL, Lausanne */

package leon
package datagen

import purescala.Common._
import purescala.Definitions._
import purescala.TreeOps._
import purescala.Trees._
import purescala.TypeTrees._
import purescala.Extractors.TopLevelAnds

import codegen.CompilationUnit
import vanuatoo.{Pattern => VPattern, _}

import evaluators._

class VanuatooDataGen(ctx: LeonContext, p: Program) extends DataGenerator {
  val unit = CompilationUnit.compileProgram(p, compileContracts = false).get

  val ints = (for (i <- Set(0, 1, 2, 3)) yield {
    i -> Constructor[Expr, TypeTree](List(), Int32Type, s => IntLiteral(i), ""+i)
  }).toMap

  val booleans = (for (b <- Set(true, false)) yield {
    b -> Constructor[Expr, TypeTree](List(), BooleanType, s => BooleanLiteral(b), ""+b)
  }).toMap

  def intConstructor(i: Int) = ints(i)

  def boolConstructor(b: Boolean) = booleans(b)

  def cPattern(c: Constructor[Expr, TypeTree], args: Seq[VPattern[Expr, TypeTree]]) = {
    ConstructorPattern[Expr, TypeTree](c, args)
  }

  private var ccConstructors = Map[CaseClassDef, Constructor[Expr, TypeTree]]()
  private var acConstructors = Map[AbstractClassDef, List[Constructor[Expr, TypeTree]]]()
  private var tConstructors  = Map[TupleType, Constructor[Expr, TypeTree]]()

  private def getConstructorFor(t: CaseClassType, act: AbstractClassType): Constructor[Expr, TypeTree] = {
    // We "up-cast" the returnType of the specific caseclass generator to match its superclass
    getConstructors(t)(0).copy(retType = act)
  }


  private def getConstructors(t: TypeTree): List[Constructor[Expr, TypeTree]] = t match {
    case tt @ TupleType(parts) =>
      List(tConstructors.getOrElse(tt, {
        val c = Constructor[Expr, TypeTree](parts, tt, s => Tuple(s).setType(tt), tt.toString)
        tConstructors += tt -> c
        c
      }))

    case act @ AbstractClassType(acd) =>
      acConstructors.getOrElse(acd, {
        val cs = acd.knownDescendents.collect {
          case ccd: CaseClassDef =>
            getConstructorFor(CaseClassType(ccd), act)
        }.toList

        acConstructors += acd -> cs

        cs
      })

    case CaseClassType(ccd) =>
      List(ccConstructors.getOrElse(ccd, {
        val c = Constructor[Expr, TypeTree](ccd.fields.map(_.tpe), CaseClassType(ccd), s => CaseClass(ccd, s), ccd.id.name)
        ccConstructors += ccd -> c
        c
      }))

    case _ =>
      ctx.reporter.error("Unknown type to generate constructor for: "+t)
      Nil
  }

  private def valueToPattern(v: AnyRef, expType: TypeTree): VPattern[Expr, TypeTree] = (v, expType) match {
    case (i: Integer, Int32Type) =>
      cPattern(intConstructor(i), List())

    case (b: java.lang.Boolean, BooleanType) =>
      cPattern(boolConstructor(b), List())

    case (cc: codegen.runtime.CaseClass, ct: ClassType) =>
      val r = cc.__getRead()

      unit.jvmClassToDef.get(cc.getClass.getName) match {
        case Some(ccd: CaseClassDef) =>
          val c = ct match {
            case act : AbstractClassType =>
              getConstructorFor(CaseClassType(ccd), act)
            case cct : CaseClassType =>
              getConstructors(CaseClassType(ccd))(0)
          }

          val fields = cc.productElements()

          val elems = for (i <- 0 until fields.length) yield {
            if (((r >> i) & 1) == 1) {
              // has been read
              valueToPattern(fields(i), ccd.fieldsIds(i).getType)
            } else {
              AnyPattern[Expr, TypeTree]()
            }
          }

          ConstructorPattern(c, elems)

        case _ =>
          sys.error("Could not retreive type for :"+cc.getClass.getName)
      }

    case (t: codegen.runtime.Tuple, tt @ TupleType(parts)) =>
      val r = t.__getRead()

      val c = getConstructors(tt)(0)

      val elems = for (i <- 0 until t.getArity) yield {
        if (((r >> i) & 1) == 1) {
          // has been read
          valueToPattern(t.get(i), parts(i))
        } else {
          AnyPattern[Expr, TypeTree]()
        }
      }

      ConstructorPattern(c, elems)

    case _ =>
      sys.error("Unsupported value, can't paternify : "+v+" : "+expType)
  }

  type InstrumentedResult = (EvaluationResults.Result, Option[vanuatoo.Pattern[Expr, TypeTree]])

  def compile(expression : Expr, argorder : Seq[Identifier]) : Option[Tuple=>InstrumentedResult] = {
    import leon.codegen.runtime.LeonCodeGenRuntimeException
    import leon.codegen.runtime.LeonCodeGenEvaluationException

    try {
      val ttype = TupleType(argorder.map(_.getType))
      val tid = FreshIdentifier("tup").setType(ttype)

      val map = argorder.zipWithIndex.map{ case (id, i) => (id -> TupleSelect(Variable(tid), i+1)) }.toMap

      val newExpr = replaceFromIDs(map, expression)

      val ce = unit.compileExpression(newExpr, Seq(tid))

      Some((args : Tuple) => {
        try {
          val jvmArgs = ce.argsToJVM(Seq(args))

          val result  = ce.evalFromJVM(jvmArgs)

          // jvmArgs is getting updated by evaluating
          val pattern = valueToPattern(jvmArgs(0), ttype)

          (EvaluationResults.Successful(result), Some(pattern))
        } catch {
          case e : ArithmeticException =>
            (EvaluationResults.RuntimeError(e.getMessage), None)

          case e : ArrayIndexOutOfBoundsException =>
            (EvaluationResults.RuntimeError(e.getMessage), None)

          case e : LeonCodeGenRuntimeException =>
            (EvaluationResults.RuntimeError(e.getMessage), None)

          case e : LeonCodeGenEvaluationException =>
            (EvaluationResults.EvaluatorError(e.getMessage), None)
        }
      })
    } catch {
      case t: Throwable =>
        ctx.reporter.warning("Error while compiling expression: "+t.getMessage)
        None
    }
  }

  def generateFor(ins: Seq[Identifier], satisfying: Expr, maxValid: Int, maxEnumerated: Int): Seq[Seq[Expr]] = {
    // Split conjunctions
    val TopLevelAnds(ands) = satisfying

    val runners = ands.map(a => compile(a, ins) match {
      case Some(runner) => Some(runner)
      case None =>
        ctx.reporter.error("Could not compile predicate "+a)
        None
    }).flatten


    val gen = new StubGenerator[Expr, TypeTree]((ints.values ++ booleans.values).toSeq, Some(getConstructors _))

    var found = Set[Seq[Expr]]()

    /**
     * Gather at most <n> isomoprhic models  before skipping them
     * - Too little means skipping many excluding patterns
     * - Too large means repetitive (and not useful models) before reaching maxEnumerated
     */

    val maxIsomorphicModels = maxValid+1;

    val it  = gen.enumerate(TupleType(ins.map(_.getType)))

    var c = 0

    while (c < maxEnumerated && found.size < maxValid && it.hasNext) {
      val model = it.next.asInstanceOf[Tuple]

      if (model eq null) {
        c = maxEnumerated
      } else {
        var failed = false;

        for (r <- runners) r(model) match {
          case (EvaluationResults.Successful(BooleanLiteral(true)), _) =>

          case (_, Some(pattern)) =>
            failed = true;

            //println("From model: "+model)
            //println("Got pattern to exclude "+pattern)

            it.exclude(pattern)

          case _ =>
        }

        if (!failed) {
          println("Got model:")
          for ((i, v) <- (ins zip model.exprs)) {
            println(" - "+i+" -> "+v)
          }


          found += model.exprs

          if (found.size % maxIsomorphicModels == 0) {
            it.skipIsomorphic()
          }
        }

        if (c % 1000 == 0) {
          println("... "+c+" ...")
        }

        c += 1
      }
    }

    found.toSeq
  }
}
