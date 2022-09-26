package com.socrata.soql.analyzer2

import scala.annotation.tailrec
import scala.language.higherKinds
import scala.util.parsing.input.{Position, NoPosition}

import com.rojoma.json.v3.ast.JString
import com.socrata.prettyprint.prelude._

import com.socrata.soql.collection._
import com.socrata.soql.environment.{ColumnName, ResourceName, TableName}
import com.socrata.soql.functions.MonomorphicFunction
import com.socrata.soql.typechecker.HasDoc

import DocUtils._

sealed abstract class Statement[+RNS, +CT, +CV] {
  type Self[+RNS, +CT, +CV] <: Statement[RNS, CT, CV]
  def asSelf: Self[RNS, CT, CV]

  val schema: OrderedMap[_ <: ColumnLabel, NameEntry[CT]]

  private[analyzer2] def realTables: Map[AutoTableLabel, DatabaseTableName]

  final def rewriteDatabaseNames(
    tableName: DatabaseTableName => DatabaseTableName,
    // This is given the _original_ database table name
    columnName: (DatabaseTableName, DatabaseColumnName) => DatabaseColumnName
  ): Self[RNS, CT, CV] =
    doRewriteDatabaseNames(new RewriteDatabaseNamesState(realTables, tableName, columnName))

  /** The names that the SoQLAnalyzer produces aren't necessarily safe
    * for use in any particular database.  This lets those
    * automatically-generated names be systematically replaced. */
  final def relabel(using: LabelProvider): Self[RNS, CT, CV] =
    doRelabel(new RelabelState(using))

  private[analyzer2] def doRelabel(state: RelabelState): Self[RNS, CT, CV]

  /** For SQL forms that can refer to the select-columns by number, replace relevant
    * entries in those forms with the relevant select-column-index.
    *
    * e.g., this will rewrite a Statement that corresponds to "select
    * x+1, count(*) group by x+1 order by count(*)" to one that
    * corresponds to "select x+1, count(*) group by 1 order by 2"
    */
  def useSelectListReferences: Self[RNS, CT, CV]
  /** Undoes `useSelectListReferences`.  Note position information may
    * not roundtrip perfectly through these two calls. */
  def unuseSelectListReferences: Self[RNS, CT, CV]

  /** Remove columns that are not useful from inner selects.
    * SelectListReferences must not be present. */
  def removeUnusedColumns: Self[RNS, CT, CV] = doRemoveUnusedColumns(columnReferences, None)

  // If "myLabel" is "None" it means "keep all output columns"
  private[analyzer2] def doRemoveUnusedColumns(used: Map[TableLabel, Set[ColumnLabel]], myLabel: Option[TableLabel]): Self[RNS, CT, CV]

  private[analyzer2] def columnReferences: Map[TableLabel, Set[ColumnLabel]]

  def isIsomorphic[RNS2 >: RNS, CT2 >: CT, CV2 >: CV](that: Statement[RNS2, CT2, CV2]): Boolean =
    findIsomorphism(new IsomorphismState, None, None, that)

  private[analyzer2] def findIsomorphism[RNS2 >: RNS, CT2 >: CT, CV2 >: CV](
    state: IsomorphismState,
    thisCurrentTableLabel: Option[TableLabel],
    thatCurrentTableLabel: Option[TableLabel],
    that: Statement[RNS2, CT2, CV2]
  ): Boolean

  private[analyzer2] def doRewriteDatabaseNames(state: RewriteDatabaseNamesState): Self[RNS, CT, CV]

  private[analyzer2] def preserveOrdering[CT2 >: CT](
    provider: LabelProvider,
    rowNumberFunction: MonomorphicFunction[CT2],
    wantOutputOrdered: Boolean,
    wantOrderingColumn: Boolean
  ): (Option[AutoColumnLabel], Self[RNS, CT2, CV])

  def find(predicate: Expr[CT, CV] => Boolean): Option[Expr[CT, CV]]
  def contains[CT2 >: CT, CV2 >: CV](e: Expr[CT2, CV2]): Boolean

  final def debugStr(implicit ev: HasDoc[CV]): String = debugStr(new StringBuilder).toString
  final def debugStr(sb: StringBuilder)(implicit ev: HasDoc[CV]): StringBuilder =
    debugDoc.layoutSmart().toStringBuilder(sb)
  def debugDoc(implicit ev: HasDoc[CV]): Doc[Annotation[RNS, CT]]

  def mapAlias[RNS2](f: Option[(RNS, ResourceName)] => Option[(RNS2, ResourceName)]): Self[RNS2, CT, CV]
}

case class CombinedTables[+RNS, +CT, +CV](
  op: TableFunc,
  left: Statement[RNS, CT, CV],
  right: Statement[RNS, CT, CV]
) extends Statement[RNS, CT, CV] {
  require(left.schema.values.map(_.typ) == right.schema.values.map(_.typ))

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

case class CTE[+RNS, +CT, +CV](
  definitionLabel: AutoTableLabel,
  definitionQuery: Statement[RNS, CT, CV],
  materializedHint: MaterializedHint,
  useQuery: Statement[RNS, CT, CV]
) extends Statement[RNS, CT, CV] {
  type Self[+RNS, +CT, +CV] = CTE[RNS, CT, CV]
  def asSelf = this

  val schema = useQuery.schema

  def find(predicate: Expr[CT, CV] => Boolean): Option[Expr[CT, CV]] =
    definitionQuery.find(predicate).orElse(useQuery.find(predicate))

  def contains[CT2 >: CT, CV2 >: CV](e: Expr[CT2, CV2]): Boolean =
    definitionQuery.contains(e) || useQuery.contains(e)

  private[analyzer2] def realTables =
    definitionQuery.realTables ++ useQuery.realTables

  private[analyzer2] def columnReferences: Map[TableLabel, Set[ColumnLabel]] =
    definitionQuery.columnReferences.mergeWith(useQuery.columnReferences)(_ ++ _)

  def useSelectListReferences = copy(definitionQuery = definitionQuery.useSelectListReferences, useQuery = useQuery.useSelectListReferences)
  def unuseSelectListReferences = copy(definitionQuery = definitionQuery.unuseSelectListReferences, useQuery = useQuery.unuseSelectListReferences)

  private[analyzer2] def doRemoveUnusedColumns(used: Map[TableLabel, Set[ColumnLabel]], myLabel: Option[TableLabel]): Self[RNS, CT, CV] =
    copy(
      definitionQuery = definitionQuery.doRemoveUnusedColumns(used, Some(definitionLabel)),
      useQuery = useQuery.doRemoveUnusedColumns(used, myLabel)
    )

  private[analyzer2] def doRewriteDatabaseNames(state: RewriteDatabaseNamesState) =
    copy(
      definitionQuery = definitionQuery.doRewriteDatabaseNames(state),
      useQuery = useQuery.doRewriteDatabaseNames(state)
    )

  private[analyzer2] def doRelabel(state: RelabelState): Self[RNS, CT, CV] =
    copy(definitionLabel = state.convert(definitionLabel),
         definitionQuery = definitionQuery.doRelabel(state),
         useQuery = useQuery.doRelabel(state))

  private[analyzer2] override def preserveOrdering[CT2 >: CT](
    provider: LabelProvider,
    rowNumberFunction: MonomorphicFunction[CT2],
    wantOutputOrdered: Boolean,
    wantOrderingColumn: Boolean
  ): (Option[AutoColumnLabel], Self[RNS, CT2, CV]) = {
    val (orderingColumn, newUseQuery) = useQuery.preserveOrdering(provider, rowNumberFunction, wantOutputOrdered, wantOrderingColumn)
    (
      orderingColumn,
      copy(
        definitionQuery = definitionQuery.preserveOrdering(provider, rowNumberFunction, false, false)._2,
        useQuery = newUseQuery
      )
    )
  }

  private[analyzer2] def findIsomorphism[RNS2 >: RNS, CT2 >: CT, CV2 >: CV](
    state: IsomorphismState,
    thisCurrentTableLabel: Option[TableLabel],
    thatCurrentTableLabel: Option[TableLabel],
    that: Statement[RNS2, CT2, CV2]
  ): Boolean =
    that match {
      case CTE(thatDefLabel, thatDefQuery, thatMatrHint, thatUseQuery) =>
        state.tryAssociate(this.definitionLabel, thatDefLabel) &&
          this.definitionQuery.findIsomorphism(state, Some(this.definitionLabel), Some(thatDefLabel), thatDefQuery) &&
          this.materializedHint == thatMatrHint &&
          this.useQuery.findIsomorphism(state, thisCurrentTableLabel, thatCurrentTableLabel, thatUseQuery)
      case _ =>
        false
    }

  def mapAlias[RNS2](f: Option[(RNS, ResourceName)] => Option[(RNS2, ResourceName)]): Self[RNS2, CT, CV] =
    copy(definitionQuery = definitionQuery.mapAlias(f), useQuery = useQuery.mapAlias(f))

  override def debugDoc(implicit ev: HasDoc[CV]): Doc[Annotation[RNS, CT]] =
    Seq(
      Seq(
        Some(d"WITH" +#+ definitionLabel.debugDoc +#+ d"AS"),
        materializedHint.debugDoc
      ).flatten.hsep,
      definitionQuery.debugDoc.encloseNesting(d"(", d")"),
      useQuery.debugDoc
    ).sep
}

case class Values[+CT, +CV](
  values: NonEmptySeq[NonEmptySeq[Expr[CT, CV]]]
) extends Statement[Nothing, CT, CV] {
  require(values.tail.forall(_.length == values.head.length))
  require(values.tail.forall(_.iterator.zip(values.head.iterator).forall { case (a, b) => a.typ == b.typ }))

  type Self[+RNS, +CT, +CV] = Values[CT, CV]
  def asSelf = this

  // This lets us see the schema with DatabaseColumnNames as keys
  def typeVariedSchema[T >: DatabaseColumnName]: OrderedMap[T, NameEntry[CT]] =
    OrderedMap() ++ values.head.iterator.zipWithIndex.map { case (expr, idx) =>
      // This is definitely a postgresqlism, unfortunately
      val name = s"column${idx+1}"
      DatabaseColumnName(name) -> NameEntry(ColumnName(name), expr.typ)
    }

  private[analyzer2] def columnReferences: Map[TableLabel, Set[ColumnLabel]] =
    Map.empty

  private[analyzer2] def doRemoveUnusedColumns(used: Map[TableLabel, Set[ColumnLabel]], myLabel: Option[TableLabel]): Self[Nothing, CT, CV] =
    this

  private[analyzer2] def findIsomorphism[RNS2, CT2 >: CT, CV2 >: CV](
    state: IsomorphismState,
    thisCurrentTableLabel: Option[TableLabel],
    thatCurrentTableLabel: Option[TableLabel],
    that: Statement[RNS2, CT2, CV2]
  ): Boolean =
    that match {
      case Values(thatValues) =>
        this.values.length == thatValues.length &&
        this.schema.size == that.schema.size &&
          this.values.iterator.zip(thatValues.iterator).forall { case (thisRow, thatRow) =>
            thisRow.iterator.zip(thatRow.iterator).forall { case (thisExpr, thatExpr) =>
              thisExpr.findIsomorphism(state, thatExpr)
            }
          }
      case _ =>
        false
    }

  val schema = typeVariedSchema

  def useSelectListReferences = this
  def unuseSelectListReferences = this

  def find(predicate: Expr[CT, CV] => Boolean): Option[Expr[CT, CV]] =
    values.iterator.flatMap(_.iterator.flatMap(_.find(predicate))).nextOption()

  def contains[CT2 >: CT, CV2 >: CV](e: Expr[CT2, CV2]): Boolean =
    values.exists(_.exists(_.contains(e)))

  def mapAlias[RNS2](f: Option[(Nothing, ResourceName)] => Option[(RNS2, ResourceName)]) = this

  private[analyzer2] def realTables = Map.empty

  private[analyzer2] def doRewriteDatabaseNames(state: RewriteDatabaseNamesState) =
    copy(
      values = values.map(_.map(_.doRewriteDatabaseNames(state)))
    )

  private[analyzer2] def doRelabel(state: RelabelState): Self[Nothing, CT, CV] =
    copy(values = values.map(_.map(_.doRelabel(state))))

  private[analyzer2] override def preserveOrdering[CT2 >: CT](
    provider: LabelProvider,
    rowNumberFunction: MonomorphicFunction[CT2],
    wantOutputOrdered: Boolean,
    wantOrderingColumn: Boolean
  ): (Option[AutoColumnLabel], Self[Nothing, CT2, CV]) = {
    // VALUES are a table and hence unordered
    (None, this)
  }

  override def debugDoc(implicit ev: HasDoc[CV]): Doc[Annotation[Nothing, CT]] = {
    Seq(
      d"VALUES",
      values.toSeq.map { row =>
        row.toSeq.zip(schema.keys).
          map { case (expr, label) =>
            expr.debugDoc.annotate(Annotation.ColumnAliasDefinition(schema(label).name, label))
          }.encloseNesting(d"(", d",", d")")
      }.encloseNesting(d"(", d",", d")")
    ).sep.nest(2)
  }
}

case class Select[+RNS, +CT, +CV](
  distinctiveness: Distinctiveness[CT, CV],
  selectList: OrderedMap[AutoColumnLabel, NamedExpr[CT, CV]],
  from: From[RNS, CT, CV],
  where: Option[Expr[CT, CV]],
  groupBy: Seq[Expr[CT, CV]],
  having: Option[Expr[CT, CV]],
  orderBy: Seq[OrderBy[CT, CV]],
  limit: Option[BigInt],
  offset: Option[BigInt],
  search: Option[String],
  hint: Set[SelectHint]
) extends Statement[RNS, CT, CV] {
  type Self[+RNS, +CT, +CV] = Select[RNS, CT, CV]
  def asSelf = this

  private[analyzer2] def columnReferences: Map[TableLabel, Set[ColumnLabel]] = {
    var refs = distinctiveness.columnReferences
    for(e <- selectList.values) {
      refs = refs.mergeWith(e.expr.columnReferences)(_ ++ _)
    }
    refs = refs.mergeWith(from.columnReferences)(_ ++ _)
    for(w <- where) {
      refs = refs.mergeWith(w.columnReferences)(_ ++ _)
    }
    for(g <- groupBy) {
      refs = refs.mergeWith(g.columnReferences)(_ ++ _)
    }
    for(h <- having) {
      refs = refs.mergeWith(h.columnReferences)(_ ++ _)
    }
    for(o <- orderBy) {
      refs = refs.mergeWith(o.expr.columnReferences)(_ ++ _)
    }
    refs
  }

  private[analyzer2] def doRemoveUnusedColumns(used: Map[TableLabel, Set[ColumnLabel]], myLabel: Option[TableLabel]): Self[RNS, CT, CV] = {
    val newSelectList = (myLabel, distinctiveness) match {
      case (_, Distinctiveness.FullyDistinct) | (None, _) =>
        // need all my columns
        selectList
      case (Some(tl), _) =>
        val wantedColumns = used.getOrElse(tl, Set.empty)
        selectList.filter { case (cl, _) => wantedColumns(cl) }
    }
    val newFrom = from.doRemoveUnusedColumns(used)
    val candidate = copy(selectList = newSelectList, from = newFrom)
    if(candidate.isAggregated != isAggregated) {
      // this is a super-extreme edge case, but consider
      //   select x.x from (select count(*), 1 as x from whatever) as x
      // Doing a naive "remove unused columns" would result in
      //  select x.x from (select 1 as x from whatever) as x
      // ..which changes the semantics of that inner query.  So, if removing
      // columns from our select list changed whether or not we're aggregated,
      // keep our column-list as-is.  This should hopefully basically never
      // happen in practice.
      copy(from = newFrom)
    } else {
      candidate
    }
  }

  final def directlyFind(predicate: Expr[CT, CV] => Boolean): Option[Expr[CT, CV]] = {
    // "directly" means "in _this_ query, not any non-lateral subqueries"
    selectList.valuesIterator.flatMap(_.expr.find(predicate)).nextOption().orElse {
      from.reduce[Option[Expr[CT, CV]]](
        Function.const(None),
        { (a, join) =>
          a.orElse {
            join.on.find(predicate)
          }.orElse {
            if(join.lateral) join.right.find(predicate) else None
          }
        })
    }.orElse {
      where.flatMap(_.find(predicate))
    }.orElse {
      groupBy.iterator.flatMap(_.find(predicate)).nextOption()
    }.orElse {
      having.flatMap(_.find(predicate))
    }.orElse {
      orderBy.iterator.flatMap(_.expr.find(predicate)).nextOption()
    }.orElse {
      distinctiveness match {
        case Distinctiveness.Indistinct => None
        case Distinctiveness.FullyDistinct => None
        case Distinctiveness.On(exprs) => exprs.iterator.flatMap(_.find(predicate)).nextOption()
      }
    }
  }

  def directlyContains[CT2 >: CT, CV2 >: CV](e: Expr[CT2, CV2]): Boolean =
    directlyFind(_ == e).isDefined

  def find(predicate: Expr[CT, CV] => Boolean): Option[Expr[CT, CV]] =
    directlyFind(predicate).orElse { // this checks everything except the non-lateral AtomicFroms
      from.reduce[Option[Expr[CT, CV]]]( // ..so that's what this does
        _.find(predicate),
        { (a, join) => a.orElse { if(join.lateral) None else join.right.find(predicate) } }
      )
    }

  def contains[CT2 >: CT, CV2 >: CV](e: Expr[CT2, CV2]): Boolean =
    find(_ == e).isDefined

  val schema = selectList.withValuesMapped { case NamedExpr(expr, name) => NameEntry(name, expr.typ) }
  lazy val selectedExprs = selectList.withValuesMapped(_.expr)

  private def freshName(base: String) = {
    val names = selectList.valuesIterator.map(_.name).toSet
    Iterator.from(1).map { i => ColumnName(base + "_" + i) }.find { n =>
      !names.contains(n)
    }.get
  }

  def isAggregated =
    groupBy.nonEmpty ||
      having.nonEmpty ||
      selectList.valuesIterator.exists(_.expr.isAggregated) ||
      orderBy.iterator.exists(_.expr.isAggregated)

  def isWindowed =
    selectList.valuesIterator.exists(_.expr.isWindowed)

  private[analyzer2] def realTables = from.realTables

  private[analyzer2] def doRewriteDatabaseNames(state: RewriteDatabaseNamesState) = {
    Select(
      distinctiveness = distinctiveness.doRewriteDatabaseNames(state),
      selectList = selectList.withValuesMapped(_.doRewriteDatabaseNames(state)),
      from = from.doRewriteDatabaseNames(state),
      where = where.map(_.doRewriteDatabaseNames(state)),
      groupBy = groupBy.map(_.doRewriteDatabaseNames(state)),
      having = having.map(_.doRewriteDatabaseNames(state)),
      orderBy = orderBy.map(_.doRewriteDatabaseNames(state)),
      limit = limit,
      offset = offset,
      search = search,
      hint = hint
    )
  }

  private[analyzer2] def doRelabel(state: RelabelState) =
    Select(
      distinctiveness = distinctiveness.doRelabel(state),
      selectList = OrderedMap() ++ selectList.iterator.map { case (k, v) => state.convert(k) -> v.doRelabel(state) },
      from = from.doRelabel(state),
      where = where.map(_.doRelabel(state)),
      groupBy = groupBy.map(_.doRelabel(state)),
      having = having.map(_.doRelabel(state)),
      orderBy = orderBy.map(_.doRelabel(state)),
      limit = limit,
      offset = offset,
      search = search,
      hint = hint
    )

  private[analyzer2] override def preserveOrdering[CT2 >: CT](
    provider: LabelProvider,
    rowNumberFunction: MonomorphicFunction[CT2],
    wantOutputOrdered: Boolean,
    wantOrderingColumn: Boolean
  ): (Option[AutoColumnLabel], Self[RNS, CT2, CV]) = {
    // If we're windowed, we want the underlying query ordered if
    // possible even if our caller doesn't care, unless there's an
    // aggregate in the way, in which case the aggregate will
    // destroy any underlying ordering anyway so we stop caring.
    val wantSubqueryOrdered = (isWindowed || wantOutputOrdered) && !isAggregated
    from.preserveOrdering(provider, rowNumberFunction, wantSubqueryOrdered, wantSubqueryOrdered) match {
      case (Some((table, column)), newFrom) =>
        val col = Column(table, column, rowNumberFunction.result)(NoPosition)

        val orderedSelf = copy(
          from = newFrom,
          orderBy = orderBy :+ OrderBy(col, true, true)
        )

        if(wantOrderingColumn) {
          val rowNumberLabel = provider.columnLabel()
          val newSelf = orderedSelf.copy(
            selectList = selectList + (rowNumberLabel -> (NamedExpr(col, freshName("order"))))
          )

          (Some(rowNumberLabel), newSelf)
        } else {
          (None, orderedSelf)
        }

      case (None, newFrom) =>
        if(wantOrderingColumn && orderBy.nonEmpty) {
          // assume the given order by provides a total order and
          // reflect that in our ordering column

          val rowNumberLabel = provider.columnLabel()
          val newSelf = copy(
            selectList = selectList + (rowNumberLabel -> NamedExpr(WindowedFunctionCall(rowNumberFunction, Nil, None, Nil, Nil, None)(NoPosition, NoPosition), freshName("order"))),
            from = newFrom
          )

          (Some(rowNumberLabel), newSelf)
        } else {
          // No ordered FROM _and_ no ORDER BY?  You don't get a column even though you asked for one
          (None, copy(from = newFrom))
        }
    }
  }

  def mapAlias[RNS2](f: Option[(RNS, ResourceName)] => Option[(RNS2, ResourceName)]): Self[RNS2, CT, CV] =
    copy(from = from.mapAlias(f))

  def useSelectListReferences: Self[RNS, CT, CV] = {
    val selectListIndices = selectList.valuesIterator.map(_.expr).toVector.zipWithIndex.reverseIterator.toMap

    def numericateExpr(e: Expr[CT, CV]): Expr[CT, CV] = {
      e match {
        case c: Column[CT] =>
          c // don't bother rewriting column references
        case e =>
          selectListIndices.get(e) match {
            case Some(idx) => SelectListReference(idx + 1, e.isAggregated, e.isWindowed, e.typ)(e.position)
            case None => e
          }
      }
    }

    copy(
      distinctiveness = distinctiveness match {
        case Distinctiveness.Indistinct | Distinctiveness.FullyDistinct => distinctiveness
        case Distinctiveness.On(exprs) => Distinctiveness.On(exprs.map(numericateExpr))
      },
      from = from.useSelectListReferences,
      groupBy = groupBy.map(numericateExpr),
      orderBy = orderBy.map { ob => ob.copy(expr = numericateExpr(ob.expr)) }
    )
  }

  def unuseSelectListReferences: Self[RNS, CT, CV] = {
    val selectListIndices = selectList.valuesIterator.map(_.expr).toVector

    def unnumericateExpr(e: Expr[CT, CV]): Expr[CT, CV] = {
      e match {
        case r@SelectListReference(idxPlusOne, _, _, _) =>
          selectListIndices(idxPlusOne - 1).reposition(r.position)
        case other =>
          other
      }
    }

    copy(
      distinctiveness = distinctiveness match {
        case Distinctiveness.Indistinct | Distinctiveness.FullyDistinct => distinctiveness
        case Distinctiveness.On(exprs) => Distinctiveness.On(exprs.map(unnumericateExpr))
      },
      from = from.useSelectListReferences,
      groupBy = groupBy.map(unnumericateExpr),
      orderBy = orderBy.map { ob => ob.copy(expr = unnumericateExpr(ob.expr)) }
    )
  }

  private[analyzer2] def findIsomorphism[RNS2 >: RNS, CT2 >: CT, CV2 >: CV](
    state: IsomorphismState,
    thisCurrentTableLabel: Option[TableLabel],
    thatCurrentTableLabel: Option[TableLabel],
    that: Statement[RNS2, CT2, CV2]
  ): Boolean =
    that match {
      case Select(
        thatDistinctiveness,
        thatSelectList,
        thatFrom,
        thatWhere,
        thatGroupBy,
        thatHaving,
        thatOrderBy,
        thatLimit,
        thatOffset,
        thatSearch,
        thatHint
      ) =>
        this.distinctiveness.findIsomorphism(state, thatDistinctiveness) &&
          this.selectList.size == thatSelectList.size &&
          this.selectList.iterator.zip(thatSelectList.iterator).forall { case ((thisColLabel, thisNamedExpr), (thatColLabel, thatNamedExpr)) =>
            state.tryAssociate(thisCurrentTableLabel, thisColLabel, thatCurrentTableLabel, thatColLabel) &&
              thisNamedExpr.expr.findIsomorphism(state, thatNamedExpr.expr)
            // do we care about the name?
          } &&
          this.from.findIsomorphism(state, thatFrom) &&
          this.where.isDefined == thatWhere.isDefined &&
          this.where.zip(thatWhere).forall { case (a, b) => a.findIsomorphism(state, b) } &&
          this.groupBy.length == thatGroupBy.length &&
          this.groupBy.zip(thatGroupBy).forall { case (a, b) => a.findIsomorphism(state, b) } &&
          this.having.isDefined == thatHaving.isDefined &&
          this.having.zip(thatHaving).forall { case (a, b) => a.findIsomorphism(state, b) } &&
          this.orderBy.length == thatOrderBy.length &&
          this.orderBy.zip(thatOrderBy).forall { case (a, b) => a.findIsomorphism(state, b) } &&
          this.limit == thatLimit &&
          this.offset == thatOffset &&
          this.search == thatSearch &&
          this.hint == thatHint
      case _ =>
        false
    }

  override def debugDoc(implicit ev: HasDoc[CV]) =
    Seq[Option[Doc[Annotation[RNS, CT]]]](
      Some(
        (Seq(Some(d"SELECT"), distinctiveness.debugDoc).flatten.hsep +:
          selectList.toSeq.zipWithIndex.map { case ((columnLabel, NamedExpr(expr, columnName)), idx) =>
            expr.debugDoc.annotate(Annotation.SelectListDefinition(idx+1)) ++ Doc.softlineSep ++ d"AS" +#+ columnLabel.debugDoc.annotate(Annotation.ColumnAliasDefinition(columnName, columnLabel))
          }.punctuate(d",")).sep.nest(2)
      ),
      Some((d"FROM" +#+ from.debugDoc).nest(2)),
      where.map { w => Seq(d"WHERE", w.debugDoc).sep.nest(2) },
      if(groupBy.nonEmpty) {
        Some((d"GROUP BY" +: groupBy.map(_.debugDoc).punctuate(d",")).sep.nest(2))
      } else {
        None
      },
      having.map { h => Seq(d"HAVING", h.debugDoc).sep.nest(2) },
      if(orderBy.nonEmpty) {
        Some((d"ORDER BY" +: orderBy.map(_.debugDoc).punctuate(d",")).sep.nest(2))
      } else {
        None
      },
      limit.map { l => d"LIMIT $l" },
      offset.map { o => d"OFFSET $o" },
      search.map { s => Seq(d"SEARCH", Doc(JString(s).toString)).sep }
    ).flatten.sep
}
