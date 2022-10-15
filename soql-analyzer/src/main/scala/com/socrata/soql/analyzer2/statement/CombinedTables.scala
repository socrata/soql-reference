package com.socrata.soql.analyzer2.statement

import com.socrata.prettyprint.prelude._

import com.socrata.soql.analyzer2._
import com.socrata.soql.analyzer2.serialization.{Readable, ReadBuffer, Writable, WriteBuffer}
import com.socrata.soql.collection._
import com.socrata.soql.environment.ResourceName
import com.socrata.soql.functions.MonomorphicFunction
import com.socrata.soql.typechecker.HasDoc

import DocUtils._

trait CombinedTablesImpl[+RNS, +CT, +CV] { this: CombinedTables[RNS, CT, CV] =>
  type Self[+RNS, +CT, +CV] = CombinedTables[RNS, CT, CV]
  def asSelf = this

  val schema = left.schema

  def find(predicate: Expr[CT, CV] => Boolean): Option[Expr[CT, CV]] =
    left.find(predicate).orElse(right.find(predicate))

  def contains[CT2 >: CT, CV2 >: CV](e: Expr[CT2, CV2]): Boolean =
    left.contains(e) || right.contains(e)

  private[analyzer2] def realTables: Map[AutoTableLabel, DatabaseTableName] =
    left.realTables ++ right.realTables

  private[analyzer2] def columnReferences: Map[TableLabel, Set[ColumnLabel]] =
    left.columnReferences.mergeWith(right.columnReferences)(_ ++ _)

  private[analyzer2] def doRewriteDatabaseNames(state: RewriteDatabaseNamesState) =
    copy(
      left = left.doRewriteDatabaseNames(state),
      right = right.doRewriteDatabaseNames(state)
    )

  private[analyzer2] def doRelabel(state: RelabelState): Self[RNS, CT, CV] =
    copy(left = left.doRelabel(state), right = right.doRelabel(state))

  private[analyzer2] def doRemoveUnusedColumns(used: Map[TableLabel, Set[ColumnLabel]], myLabel: Option[TableLabel]): Self[RNS, CT, CV] =
    // We need all the columns in our subqueries to correctly do our
    // table operation, so ignore what we're told are used and just
    // tell our subqueries "go clean yourselves up without affecting
    // your output schemas".
    copy(left = left.doRemoveUnusedColumns(used, None), right = right.doRemoveUnusedColumns(used, None))

  private[analyzer2] def findIsomorphism[RNS2 >: RNS, CT2 >: CT, CV2 >: CV](
    state: IsomorphismState,
    thisCurrentTableLabel: Option[TableLabel],
    thatCurrentTableLabel: Option[TableLabel],
    that: Statement[RNS2, CT2, CV2]
  ): Boolean =
    that match {
      case CombinedTables(_, thatLeft, thatRight) =>
        this.left.findIsomorphism(state, thisCurrentTableLabel, thatCurrentTableLabel, thatLeft) &&
          this.right.findIsomorphism(state, thisCurrentTableLabel, thatCurrentTableLabel, thatRight)
      case _ =>
        false
    }

  private[analyzer2] override def preserveOrdering[CT2 >: CT](
    provider: LabelProvider,
    rowNumberFunction: MonomorphicFunction[CT2],
    wantOutputOrdered: Boolean,
    wantOrderingColumn: Boolean
  ): (Option[AutoColumnLabel], Self[RNS, CT2, CV]) =
    (
      // table ops never preserve ordering
      None,
      copy(
        left = left.preserveOrdering(provider, rowNumberFunction, false, false)._2,
        right = right.preserveOrdering(provider, rowNumberFunction, false, false)._2
      )
    )

  def useSelectListReferences = copy(left = left.useSelectListReferences, right = right.useSelectListReferences)
  def unuseSelectListReferences = copy(left = left.unuseSelectListReferences, right = right.unuseSelectListReferences)

  def mapAlias[RNS2](f: Option[(RNS, ResourceName)] => Option[(RNS2, ResourceName)]): Self[RNS2, CT, CV] =
    copy(left = left.mapAlias(f), right = right.mapAlias(f))

  override def debugDoc(implicit ev: HasDoc[CV]): Doc[Annotation[RNS, CT]] = {
    left.debugDoc.encloseNesting(d"(", d")") +#+ op.debugDoc +#+ right.debugDoc.encloseNesting(d"(", d")")
  }
}

trait OCombinedTablesImpl { this: CombinedTables.type =>
  implicit def serialize[RNS: Writable, CT: Writable, CV](implicit ev: Writable[Expr[CT, CV]]): Writable[CombinedTables[RNS, CT, CV]] =
    new Writable[CombinedTables[RNS, CT, CV]] {
      def writeTo(buffer: WriteBuffer, ct: CombinedTables[RNS, CT, CV]): Unit = {
        buffer.write(ct.op)
        buffer.write(ct.left)
        buffer.write(ct.right)
      }
    }

  implicit def deserialize[RNS: Readable, CT: Readable, CV](implicit ev: Readable[Expr[CT, CV]]): Readable[CombinedTables[RNS, CT, CV]] =
    new Readable[CombinedTables[RNS, CT, CV]] {
      def readFrom(buffer: ReadBuffer): CombinedTables[RNS, CT, CV] = {
        CombinedTables(
          op = buffer.read[TableFunc](),
          left = buffer.read[Statement[RNS, CT, CV]](),
          right = buffer.read[Statement[RNS, CT, CV]]()
        )
      }
    }
}