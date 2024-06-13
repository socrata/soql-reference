package com.socrata.soql.functions

import org.joda.time.{DateTime, LocalDateTime, LocalDate, LocalTime, Period}
import com.vividsolutions.jts.geom.{LineString, MultiLineString, MultiPoint, MultiPolygon, Point, Polygon}

import com.socrata.soql.collection.OrderedSet
import com.socrata.soql.environment.{TypeName, Provenance, ScopedResourceName, Source}
import com.socrata.soql.typed
import com.socrata.soql.types._
import com.socrata.soql.typechecker.{TypeInfoCommon, TypeInfo, TypeInfo2}
import com.socrata.soql.ast
import com.socrata.soql.analyzer2
import com.socrata.soql.functions

import scala.util.parsing.input.{Position, NoPosition}

object SoQLTypeInfoCommon {
  private val typeParameterUniverse = OrderedSet(SoQLType.typePreferences : _*)

  private val hasType = new analyzer2.HasType[SoQLValue, SoQLType] {
    def typeOf(cv: SoQLValue) = cv.typ
  }
}

sealed trait SoQLTypeInfoCommon extends TypeInfoCommon[SoQLType, SoQLValue] {
  override final val typeParameterUniverse = SoQLTypeInfoCommon.typeParameterUniverse
  override final def typeOf(value: SoQLValue) = value.typ
  override final def typeFor(name: TypeName) = SoQLType.typesByName.get(name)
  override final def typeNameFor(typ: SoQLType): TypeName = typ.name
  override final def isOrdered(typ: SoQLType): Boolean = SoQLTypeClasses.Ordered(typ)
  override final def isBoolean(typ: SoQLType): Boolean = typ == SoQLBoolean
  override final def isGroupable(typ: SoQLType): Boolean = SoQLTypeClasses.Equatable(typ)

  // It's useful enough to say "SoQLTypeInfo.hasType" without needing
  // to fully instantiate a SoQLTypeInfo2 that I'm putting this here
  implicit def hasType = SoQLTypeInfoCommon.hasType
}

final class SoQLTypeInfo2[MT <: analyzer2.MetaTypes with ({ type ColumnType = SoQLType; type ColumnValue = SoQLValue })](val numericRowIdLiterals: Boolean) extends TypeInfo2[MT] with SoQLTypeInfoCommon with analyzer2.StatementUniverse[MT] {
  override def potentialExprs(l: ast.Literal, sourceName: Option[analyzer2.types.ScopedResourceName[MT]], primaryTable: Option[Provenance]) =
    l match {
      case ast.NullLiteral() => typeParameterUniverse.iterator.map(analyzer2.NullLiteral[MT](_)(new analyzer2.AtomicPositionInfo(Source.nonSynthetic(sourceName, l.position)))).toVector
      case ast.BooleanLiteral(b) => Seq(analyzer2.LiteralValue[MT](SoQLBoolean(b))(new analyzer2.AtomicPositionInfo(Source.nonSynthetic(sourceName, l.position))))
      case ast.NumberLiteral(n) =>
        val baseNumber = analyzer2.LiteralValue[MT](SoQLNumber(n.bigDecimal))(new analyzer2.AtomicPositionInfo(Source.nonSynthetic(sourceName, l.position)))
        Seq(
          baseNumber,
          analyzer2.FunctionCall[MT](SoQLTypeInfo.numberToMoneyFunc, Seq(baseNumber))(new analyzer2.FuncallPositionInfo(Source.Synthetic, NoPosition)),
          analyzer2.FunctionCall[MT](SoQLTypeInfo.numberToDoubleFunc, Seq(baseNumber))(new analyzer2.FuncallPositionInfo(Source.Synthetic, NoPosition))
        ) ++ (
          if(numericRowIdLiterals) {
            try {
              val id = SoQLID(n.bigDecimal.longValueExact)
              id.provenance = primaryTable
              Seq(analyzer2.LiteralValue[MT](id)(new analyzer2.AtomicPositionInfo(Source.nonSynthetic(sourceName, l.position))))
            } catch {
              case _ : ArithmeticException =>
                Nil
            }
          } else {
            Nil
          }
        )
      case ast.StringLiteral(s) =>
        val baseString = analyzer2.LiteralValue[MT](SoQLText(s))(new analyzer2.AtomicPositionInfo(Source.nonSynthetic(sourceName, l.position)))
        val results = Seq.newBuilder[analyzer2.Expr[MT]]
        results += baseString
        for {
          conversion <- SoQLTypeInfo.stringConversions
          v <- conversion.test(s, numericRowIdLiterals)
          expr <- conversion.exprs[MT]
        } {
          results += expr(v, sourceName, l.position, primaryTable).asInstanceOf[analyzer2.Expr[MT]] // SAFETY: CT and CV are the same, and that's all this cares about
        }
        results += analyzer2.FunctionCall[MT](SoQLTypeInfo.textToBlobFunc, Seq(baseString))(new analyzer2.FuncallPositionInfo(Source.Synthetic, NoPosition))
        results += analyzer2.FunctionCall[MT](SoQLTypeInfo.textToPhotoFunc, Seq(baseString))(new analyzer2.FuncallPositionInfo(Source.Synthetic, NoPosition))
        results.result()
    }

  override def boolType = SoQLBoolean.t

  override def literalBoolean(b: Boolean, source: Source) =
    analyzer2.LiteralValue[MT](SoQLBoolean(b))(new analyzer2.AtomicPositionInfo(source))

  override def updateProvenance(v: CV)(f: Provenance => Provenance): CV = {
    v match {
      case id: SoQLID =>
        val newId = id.copy()
        newId.provenance = id.provenance.map(f)
        newId
      case version: SoQLVersion =>
        val newVersion = version.copy()
        newVersion.provenance = version.provenance.map(f)
        newVersion
      case _ =>
        v
    }
  }
}

object SoQLTypeInfo extends TypeInfo[SoQLType, SoQLValue] with SoQLTypeInfoCommon {
  def soqlTypeInfo2[MT <: analyzer2.MetaTypes with ({ type ColumnType = SoQLType; type ColumnValue = SoQLValue })](numericRowIdLiterals: Boolean): TypeInfo2[MT] =
    new SoQLTypeInfo2(numericRowIdLiterals)

  override def booleanLiteralExpr(b: Boolean, pos: Position) = Seq(typed.BooleanLiteral(b, SoQLBoolean.t)(pos))

  private def getMonomorphically(f: Function[SoQLType]): MonomorphicFunction[SoQLType] =
    f.monomorphic.getOrElse(sys.error(f.identity + " not monomorphic?"))

  // This is used below in the conversions; see the comment there for
  // what it's doing.  This class exists purely so that the individual
  // functions don't _all_ need to have the generic parameter attached
  // to them.
  private class ExprHelper[MT <: analyzer2.MetaTypes with ({type ColumnType = SoQLType; type ColumnValue = SoQLValue})] extends analyzer2.StatementUniverse[MT] {
    def funcExpr(f: MonomorphicFunction) = { (t: SoQLValue, sourceName: Option[ScopedResourceName], pos: Position) =>
      analyzer2.FunctionCall[MT](f, Seq(analyzer2.LiteralValue[MT](t)(new AtomicPositionInfo(Source.nonSynthetic(sourceName, pos)))))(new FuncallPositionInfo(Source.Synthetic, NoPosition))
    }

    def textToFixedTimestampExpr(dt: DateTime, sourceName: Option[ScopedResourceName], pos: Position) =
      analyzer2.LiteralValue[MT](SoQLFixedTimestamp(dt))(new analyzer2.AtomicPositionInfo(Source.nonSynthetic(sourceName, pos)))
    def textToFloatingTimestampExpr(ldt: LocalDateTime, sourceName: Option[ScopedResourceName], pos: Position) =
      analyzer2.LiteralValue[MT](SoQLFloatingTimestamp(ldt))(new analyzer2.AtomicPositionInfo(Source.nonSynthetic(sourceName, pos)))
    def textToDateExpr(d: LocalDate, sourceName: Option[ScopedResourceName], pos: Position) =
      analyzer2.LiteralValue[MT](SoQLDate(d))(new analyzer2.AtomicPositionInfo(Source.nonSynthetic(sourceName, pos)))
    def textToTimeExpr(t: LocalTime, sourceName: Option[ScopedResourceName], pos: Position) =
      analyzer2.LiteralValue[MT](SoQLTime(t))(new analyzer2.AtomicPositionInfo(Source.nonSynthetic(sourceName, pos)))
    def textToIntervalExpr(p: Period, sourceName: Option[ScopedResourceName], pos: Position) =
      analyzer2.LiteralValue[MT](SoQLInterval(p))(new analyzer2.AtomicPositionInfo(Source.nonSynthetic(sourceName, pos)))
    def textToNumberExpr(s: SoQLText, sourceName: Option[ScopedResourceName], pos: Position) =
      analyzer2.LiteralValue[MT](SoQLNumber(new java.math.BigDecimal(s.value)))(new analyzer2.AtomicPositionInfo(Source.nonSynthetic(sourceName, pos)))
    def textToMoneyExpr(s: SoQLText, sourceName: Option[ScopedResourceName], pos: Position) =
      analyzer2.LiteralValue[MT](SoQLMoney(new java.math.BigDecimal(s.value)))(new analyzer2.AtomicPositionInfo(Source.nonSynthetic(sourceName, pos)))
    def textToPhoneExpr(phone: SoQLPhone, sourceName: Option[ScopedResourceName], pos: Position) =
      analyzer2.LiteralValue[MT](phone)(new analyzer2.AtomicPositionInfo(Source.nonSynthetic(sourceName, pos)))
    def textToUrlExpr(url: SoQLUrl, sourceName: Option[ScopedResourceName], pos: Position) =
      analyzer2.LiteralValue[MT](url)(new analyzer2.AtomicPositionInfo(Source.nonSynthetic(sourceName, pos)))
    def textToPointExpr(p: Point, sourceName: Option[ScopedResourceName], pos: Position) =
      analyzer2.LiteralValue[MT](SoQLPoint(p))(new analyzer2.AtomicPositionInfo(Source.nonSynthetic(sourceName, pos)))
    def textToMultiPointExpr(mp: MultiPoint, sourceName: Option[ScopedResourceName], pos: Position) =
      analyzer2.LiteralValue[MT](SoQLMultiPoint(mp))(new analyzer2.AtomicPositionInfo(Source.nonSynthetic(sourceName, pos)))
    def textToLineExpr(l: LineString, sourceName: Option[ScopedResourceName], pos: Position) =
      analyzer2.LiteralValue[MT](SoQLLine(l))(new analyzer2.AtomicPositionInfo(Source.nonSynthetic(sourceName, pos)))
    def textToMultiLineExpr(ml: MultiLineString, sourceName: Option[ScopedResourceName], pos: Position) =
      analyzer2.LiteralValue[MT](SoQLMultiLine(ml))(new analyzer2.AtomicPositionInfo(Source.nonSynthetic(sourceName, pos)))
    def textToPolygonExpr(p: Polygon, sourceName: Option[ScopedResourceName], pos: Position) =
      analyzer2.LiteralValue[MT](SoQLPolygon(p))(new analyzer2.AtomicPositionInfo(Source.nonSynthetic(sourceName, pos)))
    def textToMultiPolygonExpr(mp: MultiPolygon, sourceName: Option[ScopedResourceName], pos: Position) =
      analyzer2.LiteralValue[MT](SoQLMultiPolygon(mp))(new analyzer2.AtomicPositionInfo(Source.nonSynthetic(sourceName, pos)))
    def textToRowIdExpr(rid: SoQLID, sourceName: Option[ScopedResourceName], pos: Position, primaryTable: Option[Provenance]) = {
      val newRID = rid.copy()
      newRID.provenance = primaryTable
      analyzer2.LiteralValue[MT](newRID)(new analyzer2.AtomicPositionInfo(Source.nonSynthetic(sourceName, pos)))
    }
    def textToRowVersionExpr(rv: SoQLVersion, sourceName: Option[ScopedResourceName], pos: Position, primaryTable: Option[Provenance]) = {
      val newRV = rv.copy()
      newRV.provenance = primaryTable
      analyzer2.LiteralValue[MT](newRV)(new analyzer2.AtomicPositionInfo(Source.nonSynthetic(sourceName, pos)))
    }
  }

  private val textToFixedTimestampFunc = getMonomorphically(SoQLFunctions.TextToFixedTimestamp)
  private val textToFloatingTimestampFunc = getMonomorphically(SoQLFunctions.TextToFloatingTimestamp)
  private val textToDateFunc = getMonomorphically(SoQLFunctions.TextToDate)
  private val textToTimeFunc = getMonomorphically(SoQLFunctions.TextToTime)
  private val textToIntervalFunc = getMonomorphically(SoQLFunctions.TextToInterval)
  private[functions] val numberToMoneyFunc = getMonomorphically(SoQLFunctions.NumberToMoney)
  private[functions] val numberToDoubleFunc = getMonomorphically(SoQLFunctions.NumberToDouble)
  private val textToRowIdFunc = getMonomorphically(SoQLFunctions.TextToRowIdentifier)
  private val textToRowVersionFunc = getMonomorphically(SoQLFunctions.TextToRowVersion)
  private val textToNumberFunc = getMonomorphically(SoQLFunctions.TextToNumber)
  private val textToMoneyFunc = getMonomorphically(SoQLFunctions.TextToMoney)
  private val textToPointFunc = getMonomorphically(SoQLFunctions.TextToPoint)
  private val textToMultiPointFunc = getMonomorphically(SoQLFunctions.TextToMultiPoint)
  private val textToLineFunc = getMonomorphically(SoQLFunctions.TextToLine)
  private val textToMultiLineFunc = getMonomorphically(SoQLFunctions.TextToMultiLine)
  private val textToPolygonFunc = getMonomorphically(SoQLFunctions.TextToPolygon)
  private val textToMultiPolygonFunc = getMonomorphically(SoQLFunctions.TextToMultiPolygon)
  private[functions] val textToBlobFunc = getMonomorphically(SoQLFunctions.TextToBlob)
  private[functions] val textToPhotoFunc = getMonomorphically(SoQLFunctions.TextToPhoto)
  private val textToPhoneFunc = getMonomorphically(SoQLFunctions.TextToPhone)
  private val textToUrlFunc = getMonomorphically(SoQLFunctions.TextToUrl)
  private val textToLocationFunc = getMonomorphically(SoQLFunctions.TextToLocation)
  private val textToBooleanFunc = getMonomorphically(SoQLFunctions.TextToBool)

  private val numberRx = "([+-]?[0-9]+)(\\.[0-9]*)?(e[+-]?[0-9]+)?".r
  private def isNumberLiteral(s: String) = try {
    s match {
      case numberRx(_, _, _) =>
        true
      case _ =>
        false
    }
  } catch {
    case e: Exception =>
      false
  }

  private val booleanRx = "(?i)(true|false|1|0)".r
  private def textToBooleanExpr[MT <: analyzer2.MetaTypes with ({type ColumnType = SoQLType; type ColumnValue = SoQLValue})](s: SoQLText, sourceName: Option[ScopedResourceName[MT#ResourceNameScope]], pos: Position) =
    s.value.toLowerCase match { // this function could be part of ExprHelper, but I want this match close to the definition of booleanRx
      case "true"|"1" => analyzer2.LiteralValue[MT](SoQLBoolean(true))(new analyzer2.AtomicPositionInfo(Source.nonSynthetic(sourceName, pos)))
      case "false"|"0" => analyzer2.LiteralValue[MT](SoQLBoolean(false))(new analyzer2.AtomicPositionInfo(Source.nonSynthetic(sourceName, pos)))
    }
  private def isBooleanLiteral(s: String) = try {
    s match {
      case booleanRx(_) =>
        true
      case _ =>
        false
    }
  } catch {
    case e: Exception =>
      false
  }

  // This is a bit icky now; it can be simplified when/if the old type
  // tree format goes away.  It's written like this so that it can be
  // "obvious" that the old-analyzer and the new-analyzer have the
  // same set of string-literal-to-other-type implicit conversions.
  sealed private[functions] abstract class Conversions {
    // This first function attempts to convert a string literal into a
    // value that can be used to construct the relevant target type.
    // It's used to guard both analyzers
    type TestResult
    def test(s: String, numericRowIdLiterals: Boolean): Option[TestResult]

    // This produces one of the functions that will perform the
    // (runtime) conversion for old-analyzer (i.e., old-analyzer will
    // take the soql string literal '2001-01-01T00:00:00.000Z' and
    // always produce a function call expression which does a runtime
    // cast.
    val functions: Seq[MonomorphicFunction[SoQLType]]

    // This produces an analyzer2 expression with the appropriate
    // value for the target type given the positive result from the
    // test.  It sometimes will produce a runtime cast, but more
    // frequently will produce a Literal node.
    def exprs[MT <: analyzer2.MetaTypes with ({type ColumnType = SoQLType; type ColumnValue = SoQLValue})]: Seq[(TestResult, Option[ScopedResourceName[MT#ResourceNameScope]], Position, Option[Provenance]) => analyzer2.Expr[MT]]
  }

  private object Conversions {
    // A conversion type specialized to unprovenanced soql types
    sealed abstract class Unprovenanced[T] extends Conversions {
      type TestResult = T

      def tst(s: String): Option[TestResult]
      final def test(s: String, numericRowIdLiterals: Boolean) = tst(s)

      def es[MT <: analyzer2.MetaTypes with ({type ColumnType = SoQLType; type ColumnValue = SoQLValue})]: Seq[(T, Option[analyzer2.types.ScopedResourceName[MT]], Position) => analyzer2.Expr[MT]]
      final def exprs[MT <: analyzer2.MetaTypes with ({type ColumnType = SoQLType; type ColumnValue = SoQLValue})] =
        es[MT].map { ef => (t: T, sourceName: Option[analyzer2.types.ScopedResourceName[MT]], pos: Position, prov: Option[Provenance]) => ef(t, sourceName, pos) }
    }

    // A conversion type specialized to types for which the
    // intermediate value is still just the input string
    sealed abstract class Simple extends Conversions {
      type TestResult = String

      def tst(s: String): Boolean
      final def test(s: String, numericRowIdLiterals: Boolean) = if(tst(s)) Some(s) else None

      def es[MT <: analyzer2.MetaTypes with ({type ColumnType = SoQLType; type ColumnValue = SoQLValue})]: Seq[(SoQLText, Option[analyzer2.types.ScopedResourceName[MT]], Position) => analyzer2.Expr[MT]]
      final def exprs[MT <: analyzer2.MetaTypes with ({type ColumnType = SoQLType; type ColumnValue = SoQLValue})] =
        es[MT].map { ef => (s: String, sourceName: Option[analyzer2.types.ScopedResourceName[MT]], pos: Position, prov: Option[Provenance]) => ef(SoQLText(s), sourceName, pos) }
    }

    // A conversion type specialized to provenanced types
    sealed abstract class Provenanced[T] extends Conversions {
      type TestResult = T
    }
  }

  private[functions] val stringConversions = Seq[Conversions](
    new Conversions.Unprovenanced[DateTime] {
      override def tst(s: String) = SoQLFixedTimestamp.StringRep.unapply(s)
      override val functions = Seq(textToFixedTimestampFunc)
      override def es[MT <: analyzer2.MetaTypes with ({type ColumnType = SoQLType; type ColumnValue = SoQLValue})] = Seq(new ExprHelper[MT].textToFixedTimestampExpr _)
    },
    new Conversions.Unprovenanced[LocalDateTime] {
      override def tst(s: String) = SoQLFloatingTimestamp.StringRep.unapply(s)
      override val functions = Seq(textToFloatingTimestampFunc)
      override def es[MT <: analyzer2.MetaTypes with ({type ColumnType = SoQLType; type ColumnValue = SoQLValue})] = Seq(new ExprHelper[MT].textToFloatingTimestampExpr _)
    },
    new Conversions.Unprovenanced[LocalDate] {
      override def tst(s: String) = SoQLDate.StringRep.unapply(s)
      override val functions = Seq(textToDateFunc)
      override def es[MT <: analyzer2.MetaTypes with ({type ColumnType = SoQLType; type ColumnValue = SoQLValue})] = Seq(new ExprHelper[MT].textToDateExpr _)
    },
    new Conversions.Unprovenanced[LocalTime] {
      override def tst(s: String) = SoQLTime.StringRep.unapply(s)
      override val functions = Seq(textToTimeFunc)
      override def es[MT <: analyzer2.MetaTypes with ({type ColumnType = SoQLType; type ColumnValue = SoQLValue})] = Seq(new ExprHelper[MT].textToTimeExpr _)
    },
    new Conversions.Unprovenanced[Period] {
      override def tst(s: String) = SoQLInterval.StringRep.unapply(s)
      override val functions = Seq(textToIntervalFunc)
      override def es[MT <: analyzer2.MetaTypes with ({type ColumnType = SoQLType; type ColumnValue = SoQLValue})] = Seq(new ExprHelper[MT].textToIntervalExpr _)
    },
    new Conversions.Provenanced[SoQLID] {
      // This is a little unfortunate; in the analyzer1 codepath,
      // we'll never see a numeric row id literal here, because there
      // QC munges the dataset schema to turn row IDs into numbers
      // before analysis.  On the analyzer2 path, row IDs remain row
      // IDs.
      override def test(s: String, numericRowIdLiterals: Boolean) =
        if(numericRowIdLiterals) SoQLID.ClearNumberRep.unapply(s)
        else SoQLID.FormattedButUnobfuscatedStringRep.unapply(s)
      override val functions = Seq(textToRowIdFunc)
      override def exprs[MT <: analyzer2.MetaTypes with ({type ColumnType = SoQLType; type ColumnValue = SoQLValue})] = Seq(new ExprHelper[MT].textToRowIdExpr _)
    },
    new Conversions.Provenanced[SoQLVersion] {
      override def test(s: String, numericRowIdLiterals: Boolean) = SoQLVersion.FormattedButUnobfuscatedStringRep.unapply(s)
      override val functions = Seq(textToRowVersionFunc)
      override def exprs[MT <: analyzer2.MetaTypes with ({type ColumnType = SoQLType; type ColumnValue = SoQLValue})] = Seq(new ExprHelper[MT].textToRowVersionExpr _)
    },
    new Conversions.Simple {
      override def tst(s: String) = isNumberLiteral(s)
      override val functions = Seq(textToNumberFunc, textToMoneyFunc)
      override def es[MT <: analyzer2.MetaTypes with ({type ColumnType = SoQLType; type ColumnValue = SoQLValue})] = {
        val eh = new ExprHelper[MT]
        Seq(eh.textToNumberExpr _, eh.textToMoneyExpr _)
      }
    },
    new Conversions.Unprovenanced[Point] {
      override def tst(s: String) = SoQLPoint.WktRep.unapply(s)
      override val functions = Seq(textToPointFunc)
      override def es[MT <: analyzer2.MetaTypes with ({type ColumnType = SoQLType; type ColumnValue = SoQLValue})] = Seq(new ExprHelper[MT].textToPointExpr _)
    },
    new Conversions.Unprovenanced[MultiPoint] {
      override def tst(s: String) = SoQLMultiPoint.WktRep.unapply(s)
      override val functions = Seq(textToMultiPointFunc)
      override def es[MT <: analyzer2.MetaTypes with ({type ColumnType = SoQLType; type ColumnValue = SoQLValue})] = Seq(new ExprHelper[MT].textToMultiPointExpr _)
    },
    new Conversions.Unprovenanced[LineString] {
      override def tst(s: String) = SoQLLine.WktRep.unapply(s)
      override val functions = Seq(textToLineFunc)
      override def es[MT <: analyzer2.MetaTypes with ({type ColumnType = SoQLType; type ColumnValue = SoQLValue})] = Seq(new ExprHelper[MT].textToLineExpr _)
    },
    new Conversions.Unprovenanced[MultiLineString] {
      override def tst(s: String) = SoQLMultiLine.WktRep.unapply(s)
      override val functions = Seq(textToMultiLineFunc)
      override def es[MT <: analyzer2.MetaTypes with ({type ColumnType = SoQLType; type ColumnValue = SoQLValue})] = Seq(new ExprHelper[MT].textToMultiLineExpr _)
    },
    new Conversions.Unprovenanced[Polygon] {
      override def tst(s: String) = SoQLPolygon.WktRep.unapply(s)
      override val functions = Seq(textToPolygonFunc)
      override def es[MT <: analyzer2.MetaTypes with ({type ColumnType = SoQLType; type ColumnValue = SoQLValue})] = Seq(new ExprHelper[MT].textToPolygonExpr _)
    },
    new Conversions.Unprovenanced[MultiPolygon] {
      override def tst(s: String) = SoQLMultiPolygon.WktRep.unapply(s)
      override val functions = Seq(textToMultiPolygonFunc)
      override def es[MT <: analyzer2.MetaTypes with ({type ColumnType = SoQLType; type ColumnValue = SoQLValue})] = Seq(new ExprHelper[MT].textToMultiPolygonExpr _)
    },
    new Conversions.Unprovenanced[SoQLPhone] {
      override def tst(s: String) = SoQLPhone.parsePhone(s)
      override val functions = Seq(textToPhoneFunc)
      override def es[MT <: analyzer2.MetaTypes with ({type ColumnType = SoQLType; type ColumnValue = SoQLValue})] = Seq(new ExprHelper[MT].textToPhoneExpr _)
    },
    new Conversions.Unprovenanced[SoQLUrl] {
      override def tst(s: String) = SoQLUrl.parseUrl(s)
      override val functions = Seq(textToUrlFunc)
      override def es[MT <: analyzer2.MetaTypes with ({type ColumnType = SoQLType; type ColumnValue = SoQLValue})] = Seq(new ExprHelper[MT].textToUrlExpr _)
    },
    new Conversions.Simple {
      override def tst(s: String) = SoQLLocation.isPossibleLocation(s)
      override val functions = Seq(textToLocationFunc)
      override def es[MT <: analyzer2.MetaTypes with ({type ColumnType = SoQLType; type ColumnValue = SoQLValue})] = Seq(new ExprHelper[MT].funcExpr(textToLocationFunc))
    },
    new Conversions.Simple {
      override def tst(s: String) = isBooleanLiteral(s)
      override val functions = Seq(textToBooleanFunc)
      override def es[MT <: analyzer2.MetaTypes with ({type ColumnType = SoQLType; type ColumnValue = SoQLValue})] = Seq(textToBooleanExpr _)
    }
  )

  override def stringLiteralExpr(s: String, pos: Position) = {
    val baseString = typed.StringLiteral(s, SoQLText.t)(pos)
    val results = Seq.newBuilder[typed.CoreExpr[Nothing, SoQLType]]
    results += baseString
    results ++= (for {
      conversion <- stringConversions
      if conversion.test(s, false).isDefined
      func <- conversion.functions
    } yield typed.FunctionCall(func, Seq(baseString), None, None)(pos, pos))
    results += typed.FunctionCall(textToBlobFunc, Seq(baseString), None, None)(pos, pos)
    results += typed.FunctionCall(textToPhotoFunc, Seq(baseString), None, None)(pos, pos)
    results.result()
  }

  override def numberLiteralExpr(n: BigDecimal, pos: Position) = {
    val baseNumber = typed.NumberLiteral(n, SoQLNumber.t)(pos)
    Seq(
      baseNumber,
      typed.FunctionCall(numberToMoneyFunc, Seq(baseNumber), None, None)(pos, pos),
      typed.FunctionCall(numberToDoubleFunc, Seq(baseNumber), None, None)(pos, pos)
    )
  }

  override def nullLiteralExpr(pos: Position) = typeParameterUniverse.toSeq.map(typed.NullLiteral(_)(pos))

  override def literalExprFor(value: SoQLValue, pos: Position) =
    value match {
      case SoQLText(s) =>
        Some(typed.StringLiteral(s, SoQLText.t)(pos))
      case SoQLNumber(n) =>
        Some(typed.NumberLiteral(n, SoQLNumber.t)(pos))
      case SoQLBoolean(b) =>
        Some(typed.BooleanLiteral(b, SoQLBoolean.t)(pos))
      case SoQLFixedTimestamp(ts) =>
        Some(
          typed.FunctionCall(
            textToFixedTimestampFunc,
            Seq(typed.StringLiteral(SoQLFixedTimestamp.StringRep(ts), SoQLText.t)(pos)),
            None, None
          )(pos, pos)
        )
      case SoQLFloatingTimestamp(ts) =>
        Some(
          typed.FunctionCall(
            textToFloatingTimestampFunc,
            Seq(typed.StringLiteral(SoQLFloatingTimestamp.StringRep(ts), SoQLText.t)(pos)),
            None, None
          )(pos, pos)
        )
      case _ =>
        None
    }
}
