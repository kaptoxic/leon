package leon
package synthesis.ioexamples

import scala.collection.mutable.{ Map => MMap, TreeSet }

import purescala._
import Types._
import Expressions._
import Constructors._
import Definitions._
import Common._

// evaluation
import evaluators._
import solvers._

import utils.logging.HasLogger

import scala.language.postfixOps

class Decomposer(
  listTypes: (ClassDef, CaseClassDef, CaseClassDef)
  ) extends HasLogger {
  
  def decompose(
    examples: Iterable[InputOutputExample],
    fragments: List[Expr]
  ): Option[List[(List[InputOutputExample], List[Expr])]] = {
    
    val inIds = examples.head._1.map(_._1)
    val outId = examples.head._2._1
    assert(inIds.size == 1, "for now use only one variable")
    val inId = inIds.head
    
    val inputValues = examples.map(_._1.head._2)
    
    val allChange = fragments.forall({
      // TODO compare IDs here not string
//      case CaseClass(ct, args) if ct.classDef.id.name == "Change" =>
      case Hole(Untyped, Variable(id) :: _) if id.name == "change" =>
        true
      case _ =>
        false
    })
    
    if (allChange) {
      
      val changes = fragments.map(_.asInstanceOf[Hole])
      
      val (ins, outs) =
        changes.map({ x => val _ :: in :: out :: Nil = x.alts.toList; (in, out) }).unzip
      fine("inputValues:\n" + inputValues.mkString("\n"))

      val ioExamples: List[InputOutputExample] =
        for ((in, out) <- ins zip outs) yield
          ((inId, in) :: Nil, (outId, out))
          
      val initialInput = (inId, ins.head)
      val inputOutputWholeList: List[InputOutputExample] =
        ((initialInput :: Nil), (outId, makeList(outs))) :: Nil
      
      Some((ioExamples, List[Expr]()) :: (inputOutputWholeList, List[Expr]()) :: Nil)
      
    } else None
    
  }
  
  def makeList(exprs: List[Expr]): Expr = {
    val (listType, consType, nilType) = listTypes

    (CaseClass(nilType.typed, Nil) /: exprs.reverse) {
      case (res, el) =>
        CaseClass(consType.typed, el :: res :: Nil)
    }
  }


}