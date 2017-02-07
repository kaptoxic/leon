package leon
package synthesis
package ioexamples.backwards

import purescala._
import Expressions._
import Types._
import Definitions.{ FunDef, Program }
import synthesis.{ Problem, SynthesisContext }

import datagen._
import evaluators._
import codegen.CodeGenParams
import leon.grammars._
import leon.grammars.aspects._
import bonsai.enumerators._
import leon.utils.GrowableIterable
import purescala.Constructors._

import leon.utils.logging._

import scala.collection.mutable.{ HashMap => MutableMap }

// currently does not guarantee size
class TermSynthesizer(
    hctx: SynthesisContext, p: Problem,
    sizes: (Int, Int) = (1, 5),
    numOfSnippetsToSynthesize: Int = 20
) extends (Seq[TypeTree] => Iterable[Expr]) with HasLogger {
  
  def apply(tpes: Seq[TypeTree]) = {
    implicit val ci = hctx
    val grammar = grammars.default(hctx, p)
    val (minSize, maxSize) = sizes

    val enum = new MemoizedEnumerator[Label, Expr, ProductionRule[Label, Expr]](grammar.getProductions)
    
    val targetType = tupleTypeWrap(tpes)
    val rootLabel = Label(targetType).withAspect(Tagged(Tags.Top, 0, None))

    val streams = for (size <- (minSize to maxSize).toIterator) yield {
      info("Size: " + size)
      val label = rootLabel.withAspect(Sized(size, true))

      enum.iterator(label)
    }

    streams.flatten.toIterable

  }

}