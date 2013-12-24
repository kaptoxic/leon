package leon
package synthesis
package ioexamples.backwards

import scala.collection.{ mutable => m }

import purescala.Trees._
import purescala.TypeTrees._
import purescala._
import purescala.Definitions._
import purescala.Common._
import evaluators._

object AndOrGraph {
  
  // base of all nodes, declares that has children
  trait Base[T] {
    protected var children = m.LinkedList[T]()
    
    def addChild(child: T) = children :+= child
    
    def getChildren = children.toList
  }
  
  // solvable node
  trait Solvable[T] {
    def isSolved = solved
  
    private var solved = false
    
    def setSolved(n: T) = {
      solved = true
    }
    
    def unsolve(n: T) = solved = false
    
    def setSolved = {
      solved = true
    }
  }

  trait SingleSolution[T >: Null] extends Solvable[T] {
    
    private var solvedNode: T = _
    
    override def setSolved(n: T) = {
      super.setSolved(n)
      solvedNode = n
    }
    
    def getSolvedNode = solvedNode
    
    override def unsolve(n: T) = {
      super.unsolve(n)
      solvedNode = null
    }
      
  }
   
  trait Node[T] extends Base[Node[T]] with Solvable[Node[T]] {
    
//    override def unsolve = {
//      super.unsolve
//      for (child <- getChildren)
//        child.unsolve
//    }

  }

  trait WithParent[T] {
    def parent: Node[T]
  }
  
  trait SolvableWithParent[T] extends Node[T] with WithParent[T] {
    override def setSolved(n: Node[T]) = {
      super.setSolved(n)
      parent.setSolved(this)
    }

    override def unsolve(n: Node[T]) = {
      super.unsolve(n)
      parent.unsolve(this)
    }
    
  }
  
  trait AndNode[T] extends Node[T] with SolvableWithParent[T] {
    var solvedChildren = m.Set[Node[T]]()
    
    override def setSolved(n: Node[T]) = {      
      solvedChildren += n
      if (solvedChildren.size >= children.size)
        super.setSolved(n)
    }
        
    override def unsolve(n: Node[T]) = {
      super.unsolve(n)
      solvedChildren -= n
    }
    
  }
  
  trait OrNode[T] extends Node[T] with SingleSolution[Node[T]] with SolvableWithParent[T]
  
}