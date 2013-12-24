package leon
package synthesis
package ioexamples.backwards

import scala.language.implicitConversions

import scala.text.Document
import Document._
import scala.text.DocNil

import insynth.streams.Streamable
import insynth.streams.unordered._
import insynth.streams.ordered 

import insynth.util.format._
import insynth.util.format.FormatHelpers._

import scala.collection.{ mutable => m }

import purescala.Trees._
import purescala.TypeTrees._
import purescala._
import purescala.Definitions._
import purescala.Common._
import evaluators._

object AndOrGraphUtils {

	import AndOrGraph._
	import StepGraph._
	import FragmentGraph._

  def size(node: Node[_]): Int = {
    1 + node.getChildren.map( in => size(in) ).sum
  }
		
	object FormatStepGraph {
	  def apply(node: Step) = new FormatStepGraph(node, -1)
	  def apply(node: Step, withHeader: Boolean) = new FormatStepGraph(node, -1, withHeader)
	  def apply(node: Step, level: Int) = new FormatStepGraph(node, level: Int)
	}

	class FormatStepGraph(step: Step, level: Int, withHeader: Boolean = false) extends Formatable {
		import FormatHelpers._
  
		override def toDocument = trans(step, level)
		
	  def trans(node: Step, level: Int): Document = {
	
	    if (level == 0)
	      return DocNil
	      
      val childrenDocument =
        seqToDoc(node.getChildren, (s: Step) => trans(s, level - 1))
        
      val header =
      	if (withHeader) sqBrackets( node.## :: "?" :: node.isSolved.toString.substring(0, 1) )
      	else DocNil
        
	    val resDocument: Document =
		    node match {
		      case rs: RootStep =>
		        "root" :/: childrenDocument
		      case as: AndStep =>
		        "&" :: as.stepFun :/: nestedParen( childrenDocument )
		      case os: OrStep =>
		        "|" :: os.stepFun :/: {
		        	if (os.getChildren.isEmpty) DocNil
		        	else nestedBrackets( childrenDocument )
		        }
		      case _ => throw new RuntimeException//"Dont know: " :: header(node)
		    }
	    
	     header :: resDocument
	  }
	}

}