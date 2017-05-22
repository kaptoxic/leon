//package lesynth.examples
//
//import leon.purescala.Trees._
//import leon.purescala.Definitions.AbstractClassDef
//
//object Trees {
//
//  case object Leaf extends Expr with Terminal
//  case class Node(children: Seq[Expr]) extends Expr
//  case class Accessor(exp: Expr, ind: DeBruijnIndex) extends Expr
//  
//  def transformToAlgebraic(expr: Expr, hierarchy: AbstractClassDef) = {
//    def rec(expr: Expr): Expr =
//      expr match {
//        case CaseClass(classDef, Nil) =>
//          Leaf
//        case CaseClass(classDef, exprs) =>
//          Node( exprs map rec )
//      }
//    
//    rec(expr)
//  }
//
//}