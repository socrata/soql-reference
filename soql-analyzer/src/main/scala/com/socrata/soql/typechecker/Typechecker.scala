package com.socrata.soql.typechecker

import scala.util.parsing.input.Position

import com.socrata.soql.ast._
import com.socrata.soql.exceptions._
import com.socrata.soql.typed
import com.socrata.soql.environment.{ColumnName, Qualified, DatasetContext, FunctionName, ResourceName}

object Typechecker {
  type Schema[Type] = Map[ColumnName, typed.CoreExpr[Qualified[ColumnName], Type]]
  case class Ctx[Type](primarySchema: Schema[Type], schemas: Map[ResourceName, Schema[Type]])
}

class Typechecker[Type](typeInfo: TypeInfo[Type], functionInfo: FunctionInfo[Type]) extends ((Expression, Typechecker.Ctx[Type]) => typed.CoreExpr[Qualified[ColumnName], Type]) {
  import typeInfo._

  type Expr = typed.CoreExpr[Qualified[ColumnName], Type]
  type Ctx = Typechecker.Ctx[Type]
  type Schema = Typechecker.Schema[Type]

  val functionCallTypechecker = new FunctionCallTypechecker(typeInfo, functionInfo)

  def apply(e: Expression, ctx: Ctx): Expr =
    typecheck(e, ctx) match {
      case Left(tm) => throw tm
      case Right(es) => disambiguate(es)
    }

  // never returns an empty value
  private def typecheck(e: Expression, ctx: Ctx): Either[TypecheckException, Seq[Expr]] = e match {
    case r@ColumnOrAliasRef(qual, col) =>
      qual match {
        case None =>
          typecheckColRef(col, r.position, ctx.primarySchema, ctx)
        case Some(tableName) =>
          ctx.schemas.get(tableName.resourceName) match {
            case Some(schema) =>
              typecheckColRef(col, r.position, schema, ctx)
            case None =>
              Left(NoSuchTable(tableName.resourceName, r.position))
          }
      }
    case FunctionCall(SpecialFunctions.Parens, params) =>
      assert(params.length == 1, "Parens with more than one parameter?!")
      typecheck(params(0), ctx)
    case fc@FunctionCall(SpecialFunctions.Subscript, Seq(base, StringLiteral(prop))) =>
      // Subscripting special case.  Some types have "subfields" that
      // can be accessed with dot notation.  The parser turns this
      // into a subscript operator with a string literal parameter.
      // Here's where we find that that corresponds to a field access.
      // If we don't find anything that works, we pretend that we
      // never did this and just typecheck it as a subscript access.
      typecheck(base, ctx).right.flatMap { basePossibilities =>
        val asFieldAccesses =
          basePossibilities.flatMap { basePossibility =>
            val typ = basePossibility.typ
            val fnName = SpecialFunctions.Field(typeNameFor(typ), prop)
            typecheckFuncall(fc.copy(functionName = fnName, parameters = Seq(base))(fc.position, fc.functionNamePosition), ctx).right.getOrElse(Nil)
          }
        val rawSubscript = typecheckFuncall(fc, ctx)
        if(asFieldAccesses.isEmpty) {
          rawSubscript
        } else {
          rawSubscript match {
            case Left(_) => Right(asFieldAccesses)
            case Right(asSubscripts) => Right(asFieldAccesses ++ asSubscripts)
          }
        }
      }
    case fc@FunctionCall(_, _) =>
      typecheckFuncall(fc, ctx)
    case bl@BooleanLiteral(b) =>
      Right(booleanLiteralExpr(b, bl.position))
    case sl@StringLiteral(s) =>
      Right(stringLiteralExpr(s, sl.position))
    case nl@NumberLiteral(n) =>
      Right(numberLiteralExpr(n, nl.position))
    case nl@NullLiteral() =>
      Right(nullLiteralExpr(nl.position))
  }

  def typecheckColRef(col: ColumnName, position: Position, schema: Schema, ctx: Ctx): Either[TypecheckException, Seq[Expr]] =
    schema.get(col) match {
      case Some(pretyped) =>
        Right(Seq(pretyped.at(position)))
      case None =>
        Left(NoSuchColumn(col, position))
    }

  def typecheckFuncall(fc: FunctionCall, ctx: Ctx): Either[TypecheckException, Seq[Expr]] = {
    val FunctionCall(name, parameters) = fc

    val typedParameters = parameters.map(typecheck(_, ctx)).map {
      case Left(tm) => return Left(tm)
      case Right(es) => es
    }

    val options = functionInfo.functionsWithArity(name, typedParameters.length)
    if(options.isEmpty) return Left(NoSuchFunction(name, typedParameters.length, fc.functionNamePosition))
    val (failed, resolved) = divide(functionCallTypechecker.resolveOverload(options, typedParameters.map(_.map(_.typ).toSet))) {
      case Passed(f) => Right(f)
      case tm: TypeMismatchFailure[Type] => Left(tm)
    }
    if(resolved.isEmpty) {
      val TypeMismatchFailure(expected, found, idx) = failed.maxBy(_.idx)
      Left(TypeMismatch(name, typeNameFor(found.head), parameters(idx).position))
    } else {
      val potentials = resolved.flatMap { f =>
        val skipTypeCheckAfter = if (f.isWindowFunction) f.parameters.size else typedParameters.size
        val selectedParameters = (f.allParameters, typedParameters, Stream.from(0)).zipped.map { (expected, options, idx) =>
          val choices = if (idx < skipTypeCheckAfter) options.filter(_.typ == expected)
          else options.headOption.toSeq // any type is ok for window functions
          if(choices.isEmpty) sys.error("Can't happen, we passed typechecking")
          // we can't commit to a choice here.  Because if we decide this is ambiguous, we have to wait to find out
          // later if "f" is eliminated as a contender by typechecking.  It's only an error if f survives
          // typechecking.
          //
          // This means we actually need to preserve all _permutations_ of subtrees.  Fortunately they
          // won't be common -- the number of permutations is related to the number of distinct type variables
          // available to fill; otherwise unification happens and they're forced to be equal.  We don't presently
          // have any functions with more than two distinct type variables.
          choices
        }

        selectedParameters.toVector.foldRight(Seq(List.empty[Expr])) { (choices, remainingParams) =>
          choices.flatMap { choice => remainingParams.map(choice :: _) }
        }.map(typed.FunctionCall(f, _)(fc.position, fc.functionNamePosition))
      }

      // If all possibilities result in the same type, we can disambiguate here.
      // In principle, this could be
      //   potentials.groupBy(_.typ).values.map(disambiguate)
      // which would be strictly more general, but I am uncertain whether
      // that will preserve the preference-order assumption that disambiguate
      // relies on.
      val collapsed = if(potentials.forall(_.typ == potentials.head.typ)) Seq(disambiguate(potentials)) else potentials

      Right(collapsed)
    }
  }

  def divide[T, L, R](xs: TraversableOnce[T])(f: T => Either[L, R]): (Seq[L], Seq[R]) = {
    val left = Seq.newBuilder[L]
    val right = Seq.newBuilder[R]
    for(x <- xs) {
      f(x) match {
        case Left(l) => left += l
        case Right(r) => right += r
      }
    }
    (left.result(), right.result())
  }

  def disambiguate(choices: Seq[Expr]): Expr = {
    // technically we should use typeInfo.typeParameterUniverse to determine which
    // we prefer, but as it happens these are constructed such that more preferrred
    // things happen to come first anyway.
    val minSize = choices.minBy(_.size).size
    val minimal = choices.filter(_.size == minSize)
    minimal.lengthCompare(1) match {
      case 1 => minimal.head
      case n => minimal.head
    }
  }
}
