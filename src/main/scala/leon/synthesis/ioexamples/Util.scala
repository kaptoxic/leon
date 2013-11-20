package leon
package synthesis.ioexamples

import purescala.{ Trees => ot }
import purescala.Trees.{ Cons => _, Car => _, Cdr => _, _ }
import purescala.TypeTrees._
import purescala.TreeOps
import purescala.Common.FreshIdentifier
import purescala.Extractors

object Util {
  
  import Extractors._
  import TreeOps._
  
  // avoid typing problems
  val listType = ListType(BooleanType)
  
  val w = Variable(FreshIdentifier("w")).setType(listType)
  val nil = ot.NilList(listType)
  
  def Cons(h: Expr, t: Expr) = ot.Cons(h, t).setType(listType)
  def Car(l: Expr) = ot.Car(l).setType(listType)
  def Cdr(l: Expr) = ot.Cdr(l).setType(listType)
  
  def isAtom(e: Expr) = e match {
	  case Atom(_) => true
	  case _ => false
	}

  def substituteAtom(e: Expr) = e match {
	  case Atom(_) => Some(w)
	  case _ => None
	}
  
  def substituteAllAtom(e: Expr) = searchAndReplace( substituteAtom )(e)

//  def substituteAtom(e: Expr) = e match {
//	  case Atom(_) => w
//	  case _ => e
//	}

}