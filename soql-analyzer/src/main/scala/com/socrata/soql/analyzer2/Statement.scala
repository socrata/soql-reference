package com.socrata.soql.analyzer2

import scala.annotation.tailrec
import scala.language.higherKinds
import scala.util.parsing.input.{Position, NoPosition}
import scala.collection.compat.immutable.LazyList

import com.rojoma.json.v3.ast.JString
import com.socrata.prettyprint.prelude._

import com.socrata.soql.collection._
import com.socrata.soql.environment.{ColumnName, ResourceName, TableName}
import com.socrata.soql.functions.MonomorphicFunction
import com.socrata.soql.typechecker.HasDoc
import com.socrata.soql.analyzer2.serialization.{Readable, ReadBuffer, Writable, WriteBuffer}

import DocUtils._

sealed abstract class Statement[MT <: MetaTypes] extends MetaTypeHelper[MT] with LabelHelper[MT] {
  type Self[MT <: MetaTypes] <: Statement[MT]
  def asSelf: Self[MT]

  val schema: OrderedMap[_ <: ColumnLabel, NameEntry[CT]]

  // These exist because the _ on the schema makes it so you can't
  // look up a column with an arbitrary column label directly.
  def getColumn(c: ColumnLabel): Option[NameEntry[CT]]
  def column(c: ColumnLabel) = getColumn(c).get

  def unique: LazyList[Seq[ColumnLabel]]

  private[analyzer2] def realTables: Map[AutoTableLabel, DatabaseTableName]

  final def rewriteDatabaseNames(
    tableName: DatabaseTableName => DatabaseTableName,
    // This is given the _original_ database table name
    columnName: (DatabaseTableName, DatabaseColumnName) => DatabaseColumnName
  ): Self[MT] =
    doRewriteDatabaseNames(new RewriteDatabaseNamesState(realTables, tableName, columnName))

  /** The names that the SoQLAnalyzer produces aren't necessarily safe
    * for use in any particular database.  This lets those
    * automatically-generated names be systematically replaced. */
  final def relabel(using: LabelProvider): Self[MT] =
    doRelabel(new RelabelState(using))

  private[analyzer2] def doRelabel(state: RelabelState): Self[MT]

  private[analyzer2] def columnReferences: Map[TableLabel, Set[ColumnLabel]]

  def isIsomorphic(that: Statement[MT]): Boolean =
    findIsomorphism(new IsomorphismState, None, None, that)

  private[analyzer2] def findIsomorphism(
    state: IsomorphismState,
    thisCurrentTableLabel: Option[TableLabel],
    thatCurrentTableLabel: Option[TableLabel],
    that: Statement[MT]
  ): Boolean

  private[analyzer2] def doRewriteDatabaseNames(state: RewriteDatabaseNamesState): Self[MT]

  def find(predicate: Expr[MT] => Boolean): Option[Expr[MT]]
  def contains(e: Expr[MT]): Boolean

  final def debugStr(implicit ev: HasDoc[CV]): String = debugStr(new StringBuilder).toString
  final def debugStr(sb: StringBuilder)(implicit ev: HasDoc[CV]): StringBuilder =
    debugDoc.layoutSmart().toStringBuilder(sb)
  def debugDoc(implicit ev: HasDoc[CV]): Doc[Annotation[MT]]

  def mapAlias(f: Option[ResourceName] => Option[ResourceName]): Self[MT]

  final def labelMap: LabelMap[MT] = {
    val state = new LabelMapState[MT]
    doLabelMap(state)
    state.build()
  }

  private[analyzer2] def doLabelMap(state: LabelMapState[MT]): Unit
}

object Statement {
  implicit def serialize[MT <: MetaTypes](implicit writableRNS: Writable[MT#RNS], writableCT: Writable[MT#CT], writeableExpr: Writable[Expr[MT]], dtnWritable: Writable[MT#DatabaseTableNameImpl]): Writable[Statement[MT]] = new Writable[Statement[MT]] {
    def writeTo(buffer: WriteBuffer, stmt: Statement[MT]): Unit = {
      stmt match {
        case s: Select[MT] =>
          buffer.write(0)
          buffer.write(s)
        case v: Values[MT] =>
          buffer.write(1)
          buffer.write(v)
        case ct: CombinedTables[MT] =>
          buffer.write(2)
          buffer.write(ct)
        case cte: CTE[MT] =>
          buffer.write(3)
          buffer.write(cte)
      }
    }
  }

  implicit def deserialize[MT <: MetaTypes](implicit readableRNS: Readable[MT#RNS], readableCT: Readable[MT#CT], readableExpr: Readable[Expr[MT]], dtnReadable: Readable[MT#DatabaseTableNameImpl]): Readable[Statement[MT]] = new Readable[Statement[MT]] {
    def readFrom(buffer: ReadBuffer): Statement[MT] = {
      buffer.read[Int]() match {
        case 0 => buffer.read[Select[MT]]()
        case 1 => buffer.read[Values[MT]]()
        case 2 => buffer.read[CombinedTables[MT]]()
        case 3 => buffer.read[CTE[MT]]()
        case other => fail("Unknown statement tag " + other)
      }
    }
  }
}

case class CombinedTables[MT <: MetaTypes](
  op: TableFunc,
  left: Statement[MT],
  right: Statement[MT]
) extends Statement[MT] with statement.CombinedTablesImpl[MT] {
  require(left.schema.values.map(_.typ) == right.schema.values.map(_.typ))
}
object CombinedTables extends statement.OCombinedTablesImpl

case class CTE[MT <: MetaTypes](
  definitionLabel: AutoTableLabel,
  definitionAlias: Option[ResourceName], // can this ever be not-some?  If not, perhaps mapAlias's type needs changing
  definitionQuery: Statement[MT],
  materializedHint: MaterializedHint,
  useQuery: Statement[MT]
) extends Statement[MT] with statement.CTEImpl[MT]
object CTE extends statement.OCTEImpl

case class Values[MT <: MetaTypes](
  values: NonEmptySeq[NonEmptySeq[Expr[MT]]]
) extends Statement[MT] with statement.ValuesImpl[MT] {
  require(values.tail.forall(_.length == values.head.length))
  require(values.tail.forall(_.iterator.zip(values.head.iterator).forall { case (a, b) => a.typ == b.typ }))
}
object Values extends statement.OValuesImpl

case class Select[MT <: MetaTypes](
  distinctiveness: Distinctiveness[MT],
  selectList: OrderedMap[AutoColumnLabel, NamedExpr[MT]],
  from: From[MT],
  where: Option[Expr[MT]],
  groupBy: Seq[Expr[MT]],
  having: Option[Expr[MT]],
  orderBy: Seq[OrderBy[MT]],
  limit: Option[BigInt],
  offset: Option[BigInt],
  search: Option[String],
  hint: Set[SelectHint]
) extends Statement[MT] with statement.SelectImpl[MT]

object Select extends statement.OSelectImpl
