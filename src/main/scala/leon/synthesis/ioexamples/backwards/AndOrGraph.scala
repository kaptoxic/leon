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
  
  trait Base[T] {
    protected var children = m.LinkedList[T]()
    
    def addChild(child: T) = children :+= child
    
    def getChildren = children.toList
  }
  
  trait Solvable[T] {
    def isSolved = solved
  
    private var solved = false
    
    def setSolved(n: T) = {
      solved = true
    }
    
    def setSolved = {
      solved = true
    }
  }

  trait SingleSolution[T] extends Solvable[T] {
    
    private var solvedNode: T = _
    
    override def setSolved(n: T) = {
      super.setSolved(n)
      solvedNode = n
    }
    
    def getSolvedNode = solvedNode
      
  }
   
  trait Node[T] extends Base[Node[T]] with Solvable[Node[T]] 

  trait WithParent[T] {
    def parent: Node[T]
  }
  
  trait SolvableWithParent[T] extends Node[T] with WithParent[T] {
    override def setSolved(n: Node[T]) = {
      super.setSolved(n)
      parent.setSolved(this)
    }    
  }
  
  trait AndNode[T] extends Node[T] with SolvableWithParent[T] {
    var numberOfSolved = 0
    
    override def setSolved(n: Node[T]) = {      
      numberOfSolved += 1
      if (numberOfSolved >= children.size)
        super.setSolved(n)
    }
    
  }
  
  trait OrNode[T] extends Node[T] with SingleSolution[Node[T]] with SolvableWithParent[T]
  
}