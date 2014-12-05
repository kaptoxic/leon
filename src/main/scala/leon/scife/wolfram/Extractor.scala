package leon
package scife.wolfram

//import scala.reflect.runtime.universe._
//import scala.tools.reflect.ToolBox

import scala.util.parsing.combinator._
import leon.purescala.Trees._
import leon.purescala.Common._

//import scala.tools.nsc._
//import scala.reflect.internal.util._
//import scala.tools.nsc.plugins._
//
//import scala.language.implicitConversions
//
//import purescala._
//import purescala.Definitions.{
//  ClassDef  => LeonClassDef, 
//  ModuleDef => LeonModuleDef, 
//  ValDef    => LeonValDef, 
//  Import    => LeonImport,
//  _
//}
//import purescala.Trees.{Expr => LeonExpr, This => LeonThis, _}
//import purescala.TypeTrees.{TypeTree => LeonType, _}
//import purescala.Common.{ Tree => LeonTree, _ }
//import purescala.Extractors.IsTyped
//import purescala.TreeOps._
//import purescala.TypeTreeOps._
//import purescala.DefOps.{inPackage, inUnit}
//import xlang.Trees.{Block => LeonBlock, _}
//import xlang.TreeOps._
//
//import utils.{DefinedPosition, Position => LeonPosition, OffsetPosition => LeonOffsetPosition, RangePosition => LeonRangePosition}
//
//import frontends.scalac._

object Extractor extends JavaTokenParsers {
  
  def parser: Parser[List[Expr]] = repsep(constraint, "and") ^^ { _.flatten }
  
  def constraint: Parser[List[Expr]] =
    // do more complicated parser first
    expr ~ operator ~ identifier ~ operator ~ expr ^^ {
      case e1~op1~id~op2~e2 =>
        op1(e1, id) :: op2(id, e2) :: Nil
    } |
    identifier ~ operator ~ expr ^^ {
      case id~opFun~e =>
        opFun(e, id) :: Nil
    }
    
  def operator: Parser[(Expr, Expr) => Expr] =
    ( "<=" | ">=" | ">" | "<" ) ^^ {
      case "<=" => LessEquals(_, _) 
      case ">=" => GreaterEquals(_, _) 
      case "<" => LessThan(_, _) 
      case ">" => GreaterThan(_, _) 
    }
    
  def number =
    wholeNumber ^^ { n =>IntLiteral(n.toInt) }
//  |
//    floatingPointNumber ^^ {  }
  
  def identifier = ident ^^ { id => Variable(FreshIdentifier.apply(id, false)) }
    
  def factor: Parser[Expr] =
    identifier | number | "(" ~> expr <~ ")"
    
  def term  : Parser[Expr] = factor ~ rep( "*" ~ factor | "/" ~ factor) ^^ {
    case number ~ list => (number /: list) {
      case (x, "*" ~ y) => Times(x, y)
      case (x, "/" ~ y) => Division(x, y)
    }
  }
  def expr  : Parser[Expr] = term ~ rep("+" ~ log(term)("Plus term") | "-" ~ log(term)("Minus term")) ^^ {
    case number ~ list => list.foldLeft(number) { // same as before, using alternate name for /:
      case (x, "+" ~ y) => Plus(x, y)
      case (x, "-" ~ y) => Minus(x, y)
    }
  }
  
  private[wolfram] def parsePortion(parser: Parser[Any], input: String) =
    parseAll(parser, input).isInstanceOf[Success[_]]

  def parse(input: String) = {
    parseAll(parser, input) match {
      case Success(constraints, _) =>
        constraints        
      case f => throw new RuntimeException
    }
  }
  
//  def parseToScala(string: String) = {
//    
//    val tb = runtimeMirror(getClass.getClassLoader).mkToolBox()
//    
//    val tree = tb.parse(string)
//    
//    println(tree)
//    
//  }
  
//  // copy paste from Leon because of reasons
//  private def extractTree(tr: Tree)(implicit dctx: DefContext): LeonExpr = {
//      val (current, tmpRest) = tr match {
//        case Block(Block(e :: es1, l1) :: es2, l2) =>
//          (e, Some(Block(es1 ++ Seq(l1) ++ es2, l2)))
//        case Block(e :: Nil, last) =>
//          (e, Some(last))
//        case Block(e :: es, last) =>
//          (e, Some(Block(es, last)))
//        case e =>
//          (e, None)
//      }
//
//      var rest = tmpRest
//
//      val res = current match {
//        case ExArrayLiteral(tpe, args) =>
//          FiniteArray(args.map(extractTree)).setType(ArrayType(extractType(tpe)(dctx, current.pos)))
//
//        case ExCaseObject(sym) =>
//          getClassDef(sym, current.pos) match {
//            case ccd: CaseClassDef =>
//              CaseClass(CaseClassType(ccd, Seq()), Seq())
//            case _ =>
//              throw new RuntimeException(current, "Unknown case object "+sym.name)
//          }
//
//        case ExTuple(tpes, exprs) =>
//          val tupleExprs = exprs.map(e => extractTree(e))
//          val tupleType = TupleType(tupleExprs.map(expr => expr.getType))
//          Tuple(tupleExprs)
//
//        case ExErrorExpression(str, tpt) =>
//          Error(str).setType(extractType(tpt))
//
//        case ExTupleExtract(tuple, index) =>
//          val tupleExpr = extractTree(tuple)
//
//          tupleExpr.getType match {
//            case TupleType(tpes) if tpes.size >= index =>
//              TupleSelect(tupleExpr, index)
//
//            case _ =>
//              throw new RuntimeException(current, "Invalid tupple access")
//          }
//
//        case ExValDef(vs, tpt, bdy) =>
//          val binderTpe = extractType(tpt)
//          val newID = FreshIdentifier(vs.name.toString).setType(binderTpe).setOwner(currentFunDef)
//          val valTree = extractTree(bdy)
//
//          if(valTree.getType.isInstanceOf[ArrayType]) {
//            getOwner(valTree) match {
//              case None =>
//                owners += (newID -> Some(currentFunDef))
//              case _ =>
//                throw new RuntimeException("Cannot alias array")
//            }
//          }
//
//          val restTree = rest match {
//            case Some(rst) => {
//              val nctx = dctx.withNewVar(vs -> (() => Variable(newID)))
//              extractTree(rst)(nctx)
//            }
//            case None => UnitLiteral()
//          }
//
//          rest = None
//          Let(newID, valTree, restTree)
//
//
//        case ExAssign(sym, rhs) => dctx.mutableVars.get(sym) match {
//          case Some(fun) =>
//            val Variable(id) = fun()
//            val rhsTree = extractTree(rhs)
//            if(rhsTree.getType.isInstanceOf[ArrayType] && getOwner(rhsTree).isDefined) {
//              throw new RuntimeException("Cannot alias array")
//            }
//            Assignment(id, rhsTree)
//
//          case None =>
//            throw new RuntimeException("Undeclared variable.")
//        }
//
//        case wh @ ExWhile(cond, body) =>
//          val condTree = extractTree(cond)
//          val bodyTree = extractTree(body)
//          While(condTree, bodyTree)
//
//        case wh @ ExWhileWithInvariant(cond, body, inv) =>
//          val condTree = extractTree(cond)
//          val bodyTree = extractTree(body)
//          val invTree = extractTree(inv)
//
//          val w = While(condTree, bodyTree)
//          w.invariant = Some(invTree)
//          w
//
//        case epsi @ ExEpsilonExpression(tpt, varSym, predBody) =>
//          val pstpe = extractType(tpt)
//          val nctx = dctx.withNewVar(varSym -> (() => EpsilonVariable(epsi.pos).setType(pstpe)))
//          val c1 = extractTree(predBody)(nctx)
//          if(containsEpsilon(c1)) {
//            throw new RuntimeException(epsi, "Usage of nested epsilon is not allowed")
//          }
//          Epsilon(c1).setType(pstpe)
//
//        case ExWaypointExpression(tpt, i, tree) =>
//          val pstpe = extractType(tpt)
//          val IntLiteral(ri) = extractTree(i)
//          Waypoint(ri, extractTree(tree)).setType(pstpe)
//
//        case update @ ExUpdate(lhs, index, newValue) =>
//          val lhsRec = extractTree(lhs)
//          lhsRec match {
//            case Variable(_) =>
//            case _ =>
//              throw new RuntimeException("Array update only works on variables")
//          }
//
//          getOwner(lhsRec) match {
//            case Some(Some(fd)) if fd != currentFunDef =>
//              throw new RuntimeException("cannot update an array that is not defined locally")
//
//            case Some(None) =>
//              throw new RuntimeException("cannot update an array that is not defined locally")
//
//            case Some(_) =>
//
//            case None => sys.error("This array: " + lhsRec + " should have had an owner")
//          }
//
//          val indexRec = extractTree(index)
//          val newValueRec = extractTree(newValue)
//          ArrayUpdate(lhsRec, indexRec, newValueRec)
//
//        case ExInt32Literal(v) =>
//          IntLiteral(v)
//
//        case ExBooleanLiteral(v) =>
//          BooleanLiteral(v)
//
//        case ExUnitLiteral() =>
//          UnitLiteral()
//
//        case ExLocally(body) =>
//          extractTree(body)
//
//        case ExTyped(e,tpt) =>
//          // TODO: refine type here?
//          extractTree(e)
//
//        case ex @ ExIdentifier(sym, tpt) if dctx.isVariable(sym) =>
//          dctx.vars.get(sym).orElse(dctx.mutableVars.get(sym)) match {
//            case Some(builder) =>
//              builder().setPos(ex.pos)
//            case None =>
//              throw new RuntimeException("Unidentified variable "+sym+" "+sym.id+".")
//          }
//
//        case hole @ ExHoleExpression(tpt, exprs) =>
//          val leonExprs = exprs.map(extractTree)
//
//          Hole(extractType(tpt), exprs.map(extractTree))
//
//        case ops @ ExWithOracleExpression(oracles, body) =>
//          val newOracles = oracles map { case (tpt, sym) =>
//            val aTpe  = extractType(tpt)
//            val oTpe  = oracleType(ops.pos, aTpe)
//            val newID = FreshIdentifier(sym.name.toString).setType(oTpe).setOwner(currentFunDef)
//            owners += (newID -> None)
//            newID
//          }
//
//          val newVars = (oracles zip newOracles).map {
//            case ((_, sym), id) =>
//              sym -> (() => Variable(id))
//          }
//
//          val cBody = extractTree(body)(dctx.withNewVars(newVars))
//
//          WithOracle(newOracles, cBody)
//
//
//        case chs @ ExChooseExpression(args, body) =>
//          val vars = args map { case (tpt, sym) =>
//            val aTpe  = extractType(tpt)
//            val newID = FreshIdentifier(sym.name.toString).setType(aTpe).setOwner(currentFunDef)
//            owners += (newID -> None)
//            newID
//          }
//
//          val newVars = (args zip vars).map {
//            case ((_, sym), id) =>
//              sym -> (() => Variable(id))
//          }
//
//          val cBody = extractTree(body)(dctx.withNewVars(newVars))
//
//          Choose(vars, cBody)
//
//        case ExCaseClassConstruction(tpt, args) =>
//          extractType(tpt) match {
//            case cct: CaseClassType =>
//              val nargs = args.map(extractTree(_))
//              CaseClass(cct, nargs)
//
//            case _ =>
//              throw new RuntimeException("Construction of a non-case class.")
//
//          }
//
//        case ExNot(e)              => Not(extractTree(e))
//        case ExUMinus(e)           => UMinus(extractTree(e))
//
//        case ExEquals(l, r) =>
//          val rl = extractTree(l)
//          val rr = extractTree(r)
//
//          (rl.getType, rr.getType) match {
//            case (SetType(_), SetType(_)) =>
//              SetEquals(rl, rr)
//
//            case (BooleanType, BooleanType) =>
//              Iff(rl, rr)
//
//            case (rt, lt) if isSubtypeOf(rt, lt) || isSubtypeOf(lt, rt) =>
//              Equals(rl, rr)
//
//            case (rt, lt) =>
//              throw new RuntimeException("Invalid comparison: (_: "+rt+") == (_: "+lt+")")
//          }
//
//        case ExFiniteSet(tt, args)  =>
//          val underlying = extractType(tt)
//          FiniteSet(args.map(extractTree(_)).toSet).setType(SetType(underlying))
//
//        case ExFiniteMultiset(tt, args) =>
//          val underlying = extractType(tt)
//          FiniteMultiset(args.map(extractTree(_))).setType(MultisetType(underlying))
//
//        case ExEmptySet(tt) =>
//          val underlying = extractType(tt)
//          FiniteSet(Set()).setType(SetType(underlying))
//
//        case ExEmptyMultiset(tt) =>
//          val underlying = extractType(tt)
//          EmptyMultiset(underlying).setType(MultisetType(underlying))
//
//        case ExEmptyMap(ft, tt) =>
//          val fromUnderlying = extractType(ft)
//          val toUnderlying   = extractType(tt)
//          val tpe = MapType(fromUnderlying, toUnderlying)
//
//          FiniteMap(Seq()).setType(tpe)
//
//        case ExLiteralMap(ft, tt, elems) =>
//          val fromUnderlying = extractType(ft)
//          val toUnderlying   = extractType(tt)
//          val tpe = MapType(fromUnderlying, toUnderlying)
//
//          val singletons: Seq[(LeonExpr, LeonExpr)] = elems.collect {
//            case ExTuple(tpes, trees) if (trees.size == 2) =>
//              (extractTree(trees(0)), extractTree(trees(1)))
//          }
//
//          if (singletons.size != elems.size) {
//            throw new RuntimeException("Some map elements could not be extracted as Tuple2")
//          }
//
//          FiniteMap(singletons).setType(tpe)
//
//        case ExArrayFill(baseType, length, defaultValue) =>
//          val underlying = extractType(baseType)
//          val lengthRec = extractTree(length)
//          val defaultValueRec = extractTree(defaultValue)
//          ArrayFill(lengthRec, defaultValueRec).setType(ArrayType(underlying))
//
//        case ExIfThenElse(t1,t2,t3) =>
//          val r1 = extractTree(t1)
//          if(containsLetDef(r1)) {
//            throw new RuntimeException(t1, "Condition of if-then-else expression should not contain nested function definition")
//          }
//          val r2 = extractTree(t2)
//          val r3 = extractTree(t3)
//          val lub = leastUpperBound(r2.getType, r3.getType)
//          lub match {
//            case Some(lub) =>
//              IfExpr(r1, r2, r3).setType(lub)
//
//            case None =>
//              throw new RuntimeException("Both branches of ifthenelse have incompatible types ("+r2.getType.asString(ctx)+" and "+r3.getType.asString(ctx)+")")
//          }
//
//        case ExIsInstanceOf(tt, cc) => {
//          val ccRec = extractTree(cc)
//          val checkType = extractType(tt)
//          checkType match {
//            case cct @ CaseClassType(ccd, tps) => {
//              val rootType: LeonClassDef  = if(ccd.parent != None) ccd.parent.get.classDef else ccd
//
//              if(!ccRec.getType.isInstanceOf[ClassType]) {
//                throw new RuntimeException("isInstanceOf can only be used with a case class")
//              } else {
//                val testedExprType = ccRec.getType.asInstanceOf[ClassType].classDef
//                val testedExprRootType: LeonClassDef = if(testedExprType.parent != None) testedExprType.parent.get.classDef else testedExprType
//
//                if(rootType != testedExprRootType) {
//                  throw new RuntimeException("isInstanceOf can only be used with compatible case classes")
//                } else {
//                  CaseClassInstanceOf(cct, ccRec)
//                }
//              }
//            }
//            case _ => {
//              throw new RuntimeException("isInstanceOf can only be used with a case class")
//            }
//          }
//        }
//
//
//        case pm @ ExPatternMatching(sel, cses) =>
//          val rs = extractTree(sel)
//          val rc = cses.map(extractMatchCase(_))
//          val rt: LeonType = rc.map(_.rhs.getType).reduceLeft(leastUpperBound(_,_).get)
//          MatchExpr(rs, rc).setType(rt)
//
//        case t: This =>
//          extractType(t) match {
//            case ct: ClassType =>
//              LeonThis(ct)
//            case _ =>
//              throw new RuntimeException(t, "Invalid usage of `this`")
//          }
//
//        case aup @ ExArrayUpdated(ar, k, v) =>
//          val rar = extractTree(ar)
//          val rk = extractTree(k)
//          val rv = extractTree(v)
//
//          ArrayUpdated(rar, rk, rv)
//
//        case l @ ExListLiteral(tpe, elems) =>
//          val rtpe = extractType(l)
//          val cons = CaseClassType(libraryCaseClass(l.pos, "leon.collection.Cons"), Seq(rtpe));
//          val nil  = CaseClassType(libraryCaseClass(l.pos, "leon.collection.Nil"),  Seq(rtpe));
//
//          elems.foldRight(CaseClass(nil, Seq())) {
//            case (e, ls) => CaseClass(cons, Seq(extractTree(e), ls))
//          }
//
//        case chr @ ExCharLiteral(c) =>
//          CharLiteral(c)
//
//        case str @ ExStringLiteral(s) =>
//          val chars = s.toList.map(CharLiteral(_))
//          
//          val consChar = CaseClassType(libraryCaseClass(str.pos, "leon.collection.Cons"), Seq(CharType));
//          val nilChar  = CaseClassType(libraryCaseClass(str.pos, "leon.collection.Nil"),  Seq(CharType));
//
//          val charList = chars.foldRight(CaseClass(nilChar, Seq())) {
//            case (c, s) => CaseClass(consChar, Seq(c, s))
//          }
//
//          CaseClass(CaseClassType(libraryCaseClass(str.pos, "leon.lang.string.String"), Seq()), Seq(charList))
//
//        case c @ ExCall(rec, sym, tps, args) =>
//          val rrec = rec match {
//            case t if (defsToDefs contains sym) && !isMethod(sym) =>
//              null
//            case _ =>
//              extractTree(rec)
//          }
//
//          val rargs = args.map(extractTree)
//
//          //println(s"symbol $sym with id ${sym.id}")
//          //println(s"isMethod($sym) == ${isMethod(sym)}")
//          
//          
//          (rrec, sym.name.decoded, rargs) match {
//            case (null, _, args) =>
//              val fd = getFunDef(sym, c.pos)
//
//              val newTps = tps.map(t => extractType(t))
//
//              FunctionInvocation(fd.typed(newTps), args).setType(fd.returnType)
//
//            case (IsTyped(rec, ct: ClassType), _, args) if isMethod(sym) =>
//              val fd = getFunDef(sym, c.pos)
//              val cd = methodToClass(fd)
//
//              val newTps = tps.map(t => extractType(t))
//
//              MethodInvocation(rec, cd, fd.typed(newTps), args)
//
//            case (IsTyped(rec, cct: CaseClassType), name, Nil) if cct.fields.exists(_.id.name == name) =>
//
//              val fieldID = cct.fields.find(_.id.name == name).get.id
//
//              CaseClassSelector(cct, rec, fieldID)
//
//            case (a1, "!=", List(a2)) =>
//              Not(Equals(a1, a2))
//
//            // Int methods
//            case (IsTyped(a1, Int32Type), "+", List(IsTyped(a2, Int32Type))) =>
//              Plus(a1, a2)
//
//            case (IsTyped(a1, Int32Type), "-", List(IsTyped(a2, Int32Type))) =>
//              Minus(a1, a2)
//
//            case (IsTyped(a1, Int32Type), "*", List(IsTyped(a2, Int32Type))) =>
//              Times(a1, a2)
//
//            case (IsTyped(a1, Int32Type), "%", List(IsTyped(a2, Int32Type))) =>
//              Modulo(a1, a2)
//
//            case (IsTyped(a1, Int32Type), "/", List(IsTyped(a2, Int32Type))) =>
//              Division(a1, a2)
//
//            case (IsTyped(a1, Int32Type), ">", List(IsTyped(a2, Int32Type))) =>
//              GreaterThan(a1, a2)
//
//            case (IsTyped(a1, Int32Type), ">=", List(IsTyped(a2, Int32Type))) =>
//              GreaterEquals(a1, a2)
//
//            case (IsTyped(a1, Int32Type), "<", List(IsTyped(a2, Int32Type))) =>
//              LessThan(a1, a2)
//
//            case (IsTyped(a1, Int32Type), "<=", List(IsTyped(a2, Int32Type))) =>
//              LessEquals(a1, a2)
//
//            case (IsTyped(a1, Int32Type), "<=", List(IsTyped(a2, Int32Type))) =>
//              LessEquals(a1, a2)
//
//            // Boolean methods
//            case (IsTyped(a1, BooleanType), "&&", List(IsTyped(a2, BooleanType))) =>
//              And(a1, a2)
//
//            case (IsTyped(a1, BooleanType), "||", List(IsTyped(a2, BooleanType))) =>
//              Or(a1, a2)
//
//            // Set methods
//            case (IsTyped(a1, SetType(b1)), "min", Nil) =>
//              SetMin(a1).setType(b1)
//
//            case (IsTyped(a1, SetType(b1)), "max", Nil) =>
//              SetMax(a1).setType(b1)
//
//            case (IsTyped(a1, SetType(b1)), "++", List(IsTyped(a2, SetType(b2))))  if b1 == b2 =>
//              SetUnion(a1, a2).setType(SetType(b1))
//
//            case (IsTyped(a1, SetType(b1)), "&", List(IsTyped(a2, SetType(b2)))) if b1 == b2 =>
//              SetIntersection(a1, a2).setType(SetType(b1))
//
//            case (IsTyped(a1, SetType(b1)), "subsetOf", List(IsTyped(a2, SetType(b2)))) if b1 == b2 =>
//              SubsetOf(a1, a2)
//
//            case (IsTyped(a1, SetType(b1)), "--", List(IsTyped(a2, SetType(b2)))) if b1 == b2 =>
//              SetDifference(a1, a2).setType(SetType(b1))
//
//            case (IsTyped(a1, SetType(b1)), "contains", List(a2)) =>
//              ElementOfSet(a2, a1)
//
//
//            // Multiset methods
//            case (IsTyped(a1, MultisetType(b1)), "++", List(IsTyped(a2, MultisetType(b2))))  if b1 == b2 =>
//              MultisetUnion(a1, a2).setType(MultisetType(b1))
//
//            case (IsTyped(a1, MultisetType(b1)), "&", List(IsTyped(a2, MultisetType(b2)))) if b1 == b2 =>
//              MultisetIntersection(a1, a2).setType(MultisetType(b1))
//
//            case (IsTyped(a1, MultisetType(b1)), "--", List(IsTyped(a2, MultisetType(b2)))) if b1 == b2 =>
//              MultisetDifference(a1, a2).setType(MultisetType(b1))
//
//            case (IsTyped(a1, MultisetType(b1)), "+++", List(IsTyped(a2, MultisetType(b2)))) if b1 == b2 =>
//              MultisetPlus(a1, a2).setType(MultisetType(b1))
//
//            case (IsTyped(_, MultisetType(b1)), "toSet", Nil) =>
//              MultisetToSet(rrec).setType(b1)
//
//            // Array methods
//            case (IsTyped(a1, ArrayType(vt)), "apply", List(a2)) =>
//              ArraySelect(a1, a2).setType(vt)
//
//            case (IsTyped(a1, at: ArrayType), "length", Nil) =>
//              ArrayLength(a1)
//
//            case (IsTyped(a1, at: ArrayType), "clone", Nil) =>
//              ArrayClone(a1).setType(at)
//
//            case (IsTyped(a1, at: ArrayType), "updated", List(k, v)) =>
//              ArrayUpdated(a1, k, v).setType(at)
//
//
//            // Map methods
//            case (IsTyped(a1, MapType(_, vt)), "apply", List(a2)) =>
//              MapGet(a1, a2).setType(vt)
//
//            case (IsTyped(a1, mt: MapType), "isDefinedAt", List(a2)) =>
//              MapIsDefinedAt(a1, a2)
//
//            case (IsTyped(a1, mt: MapType), "contains", List(a2)) =>
//              MapIsDefinedAt(a1, a2)
//
//            case (IsTyped(a1, mt: MapType), "updated", List(k, v)) =>
//              MapUnion(a1, FiniteMap(Seq((k, v))).setType(mt)).setType(mt)
//
//            case (IsTyped(a1, mt1: MapType), "++", List(IsTyped(a2, mt2: MapType)))  if mt1 == mt2 =>
//              MapUnion(a1, a2).setType(mt1)
//              
//            case (_, name, _) =>
//              throw new RuntimeException("Unknown call to "+name)
//          }
//
//        // default behaviour is to complain :)
//        case _ =>
//          throw new RuntimeException("Could not extract as PureScala")
//      }
//
//      res.setPos(current.pos)
//
//      rest match {
//        case Some(r) =>
//          LeonBlock(Seq(res), extractTree(r))
//        case None =>
//          res
//      }
//    }
  
}