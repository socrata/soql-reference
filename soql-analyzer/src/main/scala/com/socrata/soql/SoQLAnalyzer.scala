package com.socrata.soql

import com.socrata.soql.exceptions._
import com.socrata.soql.functions.MonomorphicFunction

import scala.util.parsing.input.{NoPosition, Position}
import com.socrata.soql.aliases.AliasAnalysis
import com.socrata.soql.aggregates.AggregateChecker
import com.socrata.soql.ast._
import com.socrata.soql.parsing.{AbstractParser, Parser}
import com.socrata.soql.typechecker._
import com.socrata.soql.environment._
import com.socrata.soql.collection.OrderedMap
import com.socrata.soql.typed.Qualifier

class SoQLAnalyzer[Type](typeInfo: TypeInfo[Type],
                         functionInfo: FunctionInfo[Type],
                         parserParameters: AbstractParser.Parameters = AbstractParser.defaultParameters)
{
  type Analysis = SoQLAnalysis[ColumnName, Type]
  type Expr = typed.CoreExpr[ColumnName, Type]
  type Qualifier = String
  type AnalysisContext = Map[Qualifier, DatasetContext[Type]]

  val log = org.slf4j.LoggerFactory.getLogger(classOf[SoQLAnalyzer[_]])

  def ns2ms(ns: Long) = ns / 1000000

  val aggregateChecker = new AggregateChecker[Type]

  /** Turn a SoQL SELECT statement into a sequence of typed `Analysis` objects.
    *
    * @param query The SELECT to parse and analyze
    * @throws com.socrata.soql.exceptions.SoQLException if the query is syntactically or semantically erroneous
    * @return The analysis of the query */
  def analyzeFullQuery(query: String)(implicit ctx: AnalysisContext): List[Analysis] = {
    log.debug("Analyzing full query {}", query)
    val start = System.nanoTime()
    val parsed = new Parser(parserParameters).selectStatement(query)
    val end = System.nanoTime()
    log.trace("Parsing took {}ms", ns2ms(end - start))
    analyze(parsed)
  }

  def analyze(selects: List[Select])(implicit ctx: AnalysisContext): List[Analysis] = {
    selects.headOption match {
      case Some(firstSelect) =>
        val firstAnalysis = analyzeWithSelection(firstSelect)(ctx)
        selects.tail.scanLeft(firstAnalysis)(analyzeInOuterSelectionContext(ctx))
      case None =>
        Nil
    }
  }

  /** Turn a simple SoQL SELECT statement into an `Analysis` object.
    *
    * @param query The SELECT to parse and analyze
    * @throws com.socrata.soql.exceptions.SoQLException if the query is syntactically or semantically erroneous
    * @return The analysis of the query */
  def analyzeUnchainedQuery(query: String)(implicit ctx: AnalysisContext): Analysis = {
    log.debug("Analyzing full unchained query {}", query)
    val start = System.nanoTime()
    val parsed = new Parser(parserParameters).unchainedSelectStatement(query)
    val end = System.nanoTime()
    log.trace("Parsing took {}ms", ns2ms(end - start))

    analyzeWithSelection(parsed)(ctx)
  }

  private def baseAnalysis(implicit ctx: AnalysisContext) = {

    val selections = ctx.map { case (qual, dsCtx) =>
      val q = if (qual == TableName.PrimaryTable.qualifier) None else Some(qual)
      dsCtx.schema.transform(typed.ColumnRef(q, _, _)(NoPosition))
    }
    // TODO: Enhance resolution of column name conflict from different tables in chained SoQLs
    val mergedSelection = selections.reduce(_ ++ _)
    SoQLAnalysis(isGrouped = false,
                 distinct = false,
                 selection = mergedSelection,
                 joins = Nil,
                 where = None,
                 groupBys = Nil,
                 having = None,
                 orderBys = Nil,
                 limit = None,
                 offset = None,
                 search = None)
  }

  /** Turn framents of a SoQL SELECT statement into a typed `Analysis` object.  If no `selection` is provided,
    * one is generated based on whether the rest of the parameters indicate an aggregate query or not.  If it
    * is not an aggregate query, a selection list equivalent to `*` (i.e., every non-system column) is generated.
    * If it is an aggregate query, a selection list made up of the expressions from `groupBy` (if provided) together
    * with "`count(*)`" is generated.
    *
    * @param distinct   Reduce identical tuples into one.
    * @param selection  A selection list.
    * @param join       A join list.
    * @param where      An expression to be used as the query's WHERE clause.
    * @param groupBy    A comma-separated list of expressions to be used as the query's GROUP BY cluase.
    * @param having     An expression to be used as the query's HAVING clause.
    * @param orderBy    A comma-separated list of expressions and sort-order specifiers to be uesd as the query's ORDER BY clause.
    * @param limit      A non-negative integer to be used as the query's LIMIT parameter
    * @param offset     A non-negative integer to be used as the query's OFFSET parameter
    * @param sourceFrom The analysis-chain of the query this query is based upon, if applicable.
    * @throws com.socrata.soql.exceptions.SoQLException if the query is syntactically or semantically erroneous.
    * @return The analysis of the query.  Note this should be appended to `sourceFrom` to form a full query-chain. */
  def analyzeSplitQuery(distinct: Boolean,
                        selection: Option[String],
                        joins: Option[String],
                        where: Option[String],
                        groupBys: Option[String],
                        having: Option[String],
                        orderBys: Option[String],
                        limit: Option[String],
                        offset: Option[String],
                        search: Option[String],
                        sourceFrom: Seq[Analysis] = Nil)(implicit ctx: AnalysisContext): Analysis = {
    log.debug("analyzing split query")

    val p = new Parser(parserParameters)

    val lastQuery =
      if(sourceFrom.isEmpty) baseAnalysis
      else sourceFrom.last

    def dispatch(distinct: Boolean, selection: Option[Selection],
                 joins: List[Join],
                 where: Option[Expression], groupBys: List[Expression], having: Option[Expression], orderBys: List[OrderBy], limit: Option[BigInt], offset: Option[BigInt], search: Option[String]) =
      selection match {
        case None => analyzeNoSelectionInOuterSelectionContext(lastQuery, distinct, joins, where, groupBys, having, orderBys, limit, offset, search)
        case Some(s) => analyzeInOuterSelectionContext(ctx)(lastQuery, Select(distinct, s, joins, where, groupBys, having, orderBys, limit, offset, search))
      }

    dispatch(
      distinct,
      selection.map(p.selection),
      joins.map(p.joins).toList.flatten,
      where.map(p.expression),
      groupBys.map(p.groupBys).toList.flatten,
      having.map(p.expression),
      orderBys.map(p.orderings).toList.flatten,
      limit.map(p.limit),
      offset.map(p.offset),
      search.map(p.search)
    )
  }

  def analyzeNoSelectionInOuterSelectionContext(lastQuery: Analysis,
                                                distinct: Boolean,
                                                joins: List[Join],
                                                where: Option[Expression],
                                                groupBys: List[Expression],
                                                having: Option[Expression],
                                                orderBys: List[OrderBy],
                                                limit: Option[BigInt],
                                                offset: Option[BigInt],
                                                search: Option[String]): Analysis = {
    implicit val fakeCtx = contextFromAnalysis(lastQuery)
    analyzeNoSelection(distinct, joins, where, groupBys, having, orderBys, limit, offset, search)
  }

  private def joinCtx(joins: List[Join])(implicit ctx: AnalysisContext) = {
    joins.map(_.from).collect {
      case JoinSelect(tableName, Some(SubSelect(selects, alias))) if !SimpleSelect.isSimple(selects) =>
        val joinCtx: Map[Qualifier, DatasetContext[Type]] = ctx +
          (TableName.PrimaryTable.qualifier -> ctx(tableName.name))
        val analyses = analyze(selects)(joinCtx)
        contextFromAnalysis(alias, analyses.last)
    }
  }

  // TODO: do we need this? wat
  def analyzeNoSelection(distinct: Boolean,
                         joins: List[Join],
                         where: Option[Expression],
                         groupBys: List[Expression],
                         having: Option[Expression],
                         orderBys: List[OrderBy],
                         limit: Option[BigInt],
                         offset: Option[BigInt],
                         search: Option[String])(implicit ctx: AnalysisContext): Analysis = {
    log.debug("No selection; doing typechecking of the other parts then deciding what to make the selection")

    val ctxFromJoins = joinCtx(joins)
    val ctxWithJoins = ctx ++ ctxFromJoins
    // ok, so the only tricky thing here is the selection itself.  Since it isn't provided, there are two cases:
    //   1. If there are groupBys, having, or aggregates in orderBy, then selection should be the equivalent of
    //      selecting the group-by clauses together with "count(*)".
    //   2. Otherwise, it should be the equivalent of selecting "*".
    val typechecker = new Typechecker(typeInfo, functionInfo)(ctxWithJoins)
    val subscriptConverter = new SubscriptConverter(typeInfo, functionInfo)

    // Rewrite subscript before typecheck
    val typecheck = subscriptConverter andThen (e => typechecker(e, Map.empty))

    val t0 = System.nanoTime()
    val checkedWhere = where.map(typecheck)
    val t1 = System.nanoTime()
    val checkedGroupBy = groupBys.map(typecheck)
    val t2 = System.nanoTime()
    val checkedHaving = having.map(typecheck)
    val t3 = System.nanoTime()
    val checkedOrderBy = orderBys.map { ob => ob -> typecheck(ob.expression) }
    val t4 = System.nanoTime()
    val isGrouped = aggregateChecker(Nil, checkedWhere, checkedGroupBy, checkedHaving, checkedOrderBy.map(_._2))
    val t5 = System.nanoTime()

    if(log.isTraceEnabled) {
      log.trace("typechecking WHERE took {}ms", ns2ms(t1 - t0))
      log.trace("typechecking GROUP BY took {}ms", ns2ms(t2 - t1))
      log.trace("typechecking HAVING took {}ms", ns2ms(t3 - t2))
      log.trace("typechecking ORDER BY took {}ms", ns2ms(t4 - t3))
      log.trace("checking for aggregation took {}ms", ns2ms(t5 - t4))
    }

    val (names, typedSelectedExpressions) = if(isGrouped) {
      log.debug("It is grouped; selecting the group bys plus count(*)")

      val beforeAliasAnalysis = System.nanoTime()
      val count_* = FunctionCall(SpecialFunctions.StarFunc("count"), Nil)(NoPosition, NoPosition)
      val untypedSelectedExpressions = groupBys :+ count_*
      val names = AliasAnalysis(Selection(None, Seq.empty, untypedSelectedExpressions.map(SelectedExpression(_, None)))).expressions.keys.toSeq
      val afterAliasAnalysis = System.nanoTime()

      log.trace("alias analysis took {}ms", ns2ms(afterAliasAnalysis - beforeAliasAnalysis))

      val typedSelectedExpressions = checkedGroupBy :+ typechecker(count_*, Map.empty)

      (names, typedSelectedExpressions)
    } else { // ok, no group by...
      log.debug("It is not grouped; selecting *")

      val beforeAliasAnalysis = System.nanoTime()
      val names = AliasAnalysis(Selection(None, Seq(StarSelection(None, Nil)), Nil)).expressions.keys.toSeq
      val afterAliasAnalyis = System.nanoTime()

      log.trace("alias analysis took {}ms", ns2ms(afterAliasAnalyis - beforeAliasAnalysis))

      val typedSelectedExpressions = names.map { column =>
        typed.ColumnRef(None, column, ctx(TableName.PrimaryTable.qualifier).schema(column))(NoPosition)
      }

      (names, typedSelectedExpressions)
    }

    val checkedJoin = joins.map { j: Join =>
      val subAnalysisOpt = subAnalysis(j)(ctx)
      typed.Join(j.typ, JoinAnalysis(j.from.fromTable, subAnalysisOpt), typecheck(j.on))
    }

    finishAnalysis(
      isGrouped,
      distinct,
      OrderedMap(names.zip(typedSelectedExpressions): _*),
      checkedJoin,
      checkedWhere,
      checkedGroupBy,
      checkedHaving,
      checkedOrderBy,
      limit,
      offset,
      search)
  }

  def analyzeInOuterSelectionContext(initialCtx: AnalysisContext)(lastQuery: Analysis, query: Select): Analysis = {
    val prevCtx = contextFromAnalysis(lastQuery)
    val nextCtx = prevCtx ++ initialCtx.filterKeys(qualifier => TableName.PrimaryTable.qualifier != qualifier)
    analyzeWithSelection(query)(nextCtx)
  }

  def contextFromAnalysis(a: Analysis): AnalysisContext = {
    val ctx = new DatasetContext[Type] {
      override val schema: OrderedMap[ColumnName, Type] = a.selection.mapValues(_.typ)
    }
    Map(TableName.PrimaryTable.qualifier -> ctx)
  }

  private def contextFromAnalysis(qualifier: Qualifier, a: Analysis) = {
    val ctx = new DatasetContext[Type] {
      override val schema: OrderedMap[ColumnName, Type] = a.selection.mapValues(_.typ)
    }
    (qualifier -> ctx)
  }

  def subAnalysis(join: Join)(ctx: AnalysisContext) = {
    join.from.subSelect.map {
      case SubSelect(selects, alias) =>
        val joinCtx: Map[Qualifier, DatasetContext[Type]] = ctx +
          (TableName.PrimaryTable.qualifier -> ctx(join.from.fromTable.name))
        val analyses = analyze(selects)(joinCtx)
        SubAnalysis(analyses, alias)
    }
  }

  def analyzeWithSelection(query: Select)(implicit ctx: AnalysisContext): Analysis = {
    log.debug("There is a selection; typechecking all parts")

    val ctxFromJoins = joinCtx(query.joins)
    val ctxWithJoins = ctx ++ ctxFromJoins
    val typechecker = new Typechecker(typeInfo, functionInfo)(ctxWithJoins)

    val subscriptConverter = new SubscriptConverter(typeInfo, functionInfo)

    val t0 = System.nanoTime()
    val aliasAnalysis = AliasAnalysis(query.selection)
    val t1 = System.nanoTime()
    val typedAliases = aliasAnalysis.evaluationOrder.foldLeft(Map.empty[ColumnName, Expr]) { (acc, alias) =>
      acc + (alias -> typechecker(subscriptConverter(aliasAnalysis.expressions(alias)), acc))
    }

    // Rewrite subscript before typecheck
    val typecheck = subscriptConverter andThen (e => typechecker(e, typedAliases))

    val checkedJoin = query.joins.map { j: Join =>
      val subAnalysisOpt = subAnalysis(j)(ctx)
      typed.Join(j.typ, JoinAnalysis(j.from.fromTable, subAnalysisOpt), typecheck(j.on))
    }

    val outputs = OrderedMap(aliasAnalysis.expressions.keys.map { k => k -> typedAliases(k) }.toList : _*)
    val t2 = System.nanoTime()
    val checkedWhere = query.where.map(typecheck)
    val t3 = System.nanoTime()
    val checkedGroupBy = query.groupBys.map(typecheck)
    val t4 = System.nanoTime()
    val checkedHaving = query.having.map(typecheck)
    val t5 = System.nanoTime()
    val checkedOrderBy = query.orderBys.map { ob => ob -> typecheck(ob.expression) }
    val t6 = System.nanoTime()
    val isGrouped = aggregateChecker(
      outputs.values.toList,
      checkedWhere,
      checkedGroupBy,
      checkedHaving,
      checkedOrderBy.map(_._2))
    val t7 = System.nanoTime()

    if(log.isTraceEnabled) {
      log.trace("alias analysis took {}ms", ns2ms(t1 - t0))
      log.trace("typechecking aliases took {}ms", ns2ms(t2 - t1))
      log.trace("typechecking WHERE took {}ms", ns2ms(t3 - t2))
      log.trace("typechecking GROUP BY took {}ms", ns2ms(t4 - t3))
      log.trace("typechecking HAVING took {}ms", ns2ms(t5 - t4))
      log.trace("typechecking ORDER BY took {}ms", ns2ms(t6 - t5))
      log.trace("checking for aggregation took {}ms", ns2ms(t7 - t6))
    }

    finishAnalysis(isGrouped, query.distinct,
                   outputs,
                   checkedJoin, checkedWhere, checkedGroupBy, checkedHaving, checkedOrderBy,
                   query.limit, query.offset, query.search)
  }

  def finishAnalysis(isGrouped: Boolean,
                     distinct: Boolean,
                     output: OrderedMap[ColumnName, Expr],
                     joins: List[typed.Join[ColumnName, Type]],
                     where: Option[Expr],
                     groupBys: List[Expr],
                     having: Option[Expr],
                     orderBys: List[(OrderBy, Expr)],
                     limit: Option[BigInt],
                     offset: Option[BigInt],
                     search: Option[String]): Analysis =
  {
    def check(items: TraversableOnce[Expr], pred: Type => Boolean, onError: (TypeName, Position) => Throwable) {
      for(item <- items if !pred(item.typ)) throw onError(typeInfo.typeNameFor(item.typ), item.position)
    }

    check(where, typeInfo.isBoolean, NonBooleanWhere)
    check(groupBys, typeInfo.isGroupable, NonGroupableGroupBy)
    check(having, typeInfo.isBoolean, NonBooleanHaving)
    check(orderBys.map(_._2), typeInfo.isOrdered, UnorderableOrderBy)

    SoQLAnalysis(
      isGrouped,
      distinct,
      output,
      joins,
      where,
      groupBys,
      having,
      orderBys.map { case (ob, e) => typed.OrderBy(e, ob.ascending, ob.nullLast) },
      limit,
      offset,
      search)
  }
}

case class SubAnalysis[ColumnId, Type](analyses: List[SoQLAnalysis[ColumnId, Type]], alias: String)
case class JoinAnalysis[ColumnId, Type](fromTable: TableName, subAnalysis: Option[SubAnalysis[ColumnId, Type]]) {
  def aliasOpt: Option[String] =  subAnalysis.map(_.alias).orElse(fromTable.alias)
  def analyses: List[SoQLAnalysis[ColumnId, Type]] = subAnalysis.map(_.analyses).getOrElse(Nil)
}

object SoQLAnalyzer {
  type TopLevelAnalysis[ColumnId, Type] = List[SoQLAnalysis[ColumnId, Type]]
}

/**
  * @param isGrouped true iff there is a group by or aggregation function applied.  Can be derived from the selection
  *                  and the groupBy
  */
case class SoQLAnalysis[ColumnId, Type](isGrouped: Boolean,
                                        distinct: Boolean,
                                        selection: OrderedMap[ColumnName, typed.CoreExpr[ColumnId, Type]],
                                        joins: List[typed.Join[ColumnId, Type]],
                                        where: Option[typed.CoreExpr[ColumnId, Type]],
                                        groupBys: List[typed.CoreExpr[ColumnId, Type]],
                                        having: Option[typed.CoreExpr[ColumnId, Type]],
                                        orderBys: List[typed.OrderBy[ColumnId, Type]],
                                        limit: Option[BigInt],
                                        offset: Option[BigInt],
                                        search: Option[String]) {
  def mapColumnIds[NewColumnId](f: (ColumnId, Qualifier) => NewColumnId): SoQLAnalysis[NewColumnId, Type] =
    copy(
      selection = selection.mapValues(_.mapColumnIds(f)),
      joins = joins.map(_.mapColumnIds(f)),
      where = where.map(_.mapColumnIds(f)),
      groupBys = groupBys.map(_.mapColumnIds(f)),
      having = having.map(_.mapColumnIds(f)),
      orderBys = orderBys.map(_.mapColumnIds(f))
    )

  def mapColumnIds[NewColumnId](qColumnIdNewColumnIdMap: Map[(ColumnId, Qualifier), NewColumnId],
                                qColumnNameToQColumnId: (Qualifier, ColumnName) => (ColumnId, Qualifier),
                                columnNameToNewColumnId: ColumnName => NewColumnId,
                                columnIdToNewColumnId: ColumnId => NewColumnId): SoQLAnalysis[NewColumnId, Type] = {
    val newColumnsFromJoin = joins.flatMap { j =>
      j.from.analyses.lastOption.flatMap(_.selection.map { case (columnName, _) =>
        qColumnNameToQColumnId(j.from.aliasOpt, columnName) -> columnNameToNewColumnId(columnName)
      })
    }.toMap

    val qColumnIdNewColumnIdWithJoinsMap = qColumnIdNewColumnIdMap ++ newColumnsFromJoin

    copy(
      selection = selection.mapValues(_.mapColumnIds(Function.untupled(qColumnIdNewColumnIdWithJoinsMap))),
      join = join.map { joins => joins.map { j =>
        val analysesInColIds = j.tableLike.foldLeft(Seq.empty[SoQLAnalysis[NewColumnId, Type]]) { (convertedAnalyses, analysis) =>
          val joinMap =
            convertedAnalyses.lastOption match {
              case None =>
                qColumnIdNewColumnIdMap.foldLeft(Map.empty[(ColumnId, Qualifier), NewColumnId]) { (acc, kv) =>
                  val ((cid, qual), ncid) = kv
                  if (qual == j.tableLike.head.from) acc + ((cid, None) -> ncid)
                  else acc
                }
              case Some(prevAnalysis) =>
                prevAnalysis.selection.foldLeft(qColumnIdNewColumnIdMap) { (acc, selCol) =>
                  val (colName, expr) = selCol
                  acc + (qColumnNameToQColumnId(None, colName) -> columnNameToNewColumnId(colName))
                }
            }

          val a = analysis.mapColumnIds(joinMap, qColumnNameToQColumnId, columnNameToNewColumnId, columnIdToNewColumnId)
          convertedAnalyses :+ a
        }

        val remappedJoin = analysesInColIds
        typed.Join(j.typ, remappedJoin, j.alias, j.expr.mapColumnIds(Function.untupled(qColumnIdNewColumnIdWithJoinsMap)))
      }},
      where = where.map(_.mapColumnIds(Function.untupled(qColumnIdNewColumnIdWithJoinsMap))),
      groupBy = groupBy.map(_.map(_.mapColumnIds(Function.untupled(qColumnIdNewColumnIdWithJoinsMap)))),
      having = having.map(_.mapColumnIds(Function.untupled(qColumnIdNewColumnIdWithJoinsMap))),
      orderBy = orderBy.map(_.map(_.mapColumnIds(Function.untupled(qColumnIdNewColumnIdWithJoinsMap))))
    )
  }
}

object SoQLAnalysis {
  def merge[T](andFunction: MonomorphicFunction[T], stages: Seq[SoQLAnalysis[ColumnName, T]]): Seq[SoQLAnalysis[ColumnName, T]] =
    new Merger(andFunction).merge(stages)
}

object SimpleSoQLAnalysis {
  def apply[ColumnId, Type](from: String): SoQLAnalysis[ColumnId, Type] = {
    SoQLAnalysis(isGrouped = false,
                 distinct = false,
                 selection = OrderedMap.empty,
                 from = Some(from),
                 join = None,
                 where = None,
                 groupBy = None,
                 having = None,
                 orderBy = None,
                 limit = None,
                 offset = None,
                 search = None)
  }

  /**
    * Simple SoQLAnalysis is a soql analysis created by a join where a sub-query is not used like "JOIN @aaaa-aaaa"
    */
  def isSimple[ColumnId, Type](a: SoQLAnalysis[ColumnId, Type]): Boolean = {
    a.selection.keys.isEmpty && a.from.nonEmpty
  }

  def isSimple[ColumnId, Type](a: Seq[SoQLAnalysis[ColumnId, Type]]): Boolean = {
    a.nonEmpty && isSimple(a.last)
  }

  def asSoQL[ColumnId, Type](a: Seq[SoQLAnalysis[ColumnId, Type]]): Option[String] = {
    if (isSimple(a)) {
      a.last.from
    } else {
      None
    }
  }
}

private class Merger[T](andFunction: MonomorphicFunction[T]) {
  type Analysis = SoQLAnalysis[ColumnName, T]
  type Expr = typed.CoreExpr[ColumnName, T]

  def merge(stages: Seq[Analysis]): Seq[Analysis] =
    stages.drop(1).foldLeft(stages.take(1).toList) { (acc, nextStage) =>
      tryMerge(acc.head /* acc cannot be empty */, nextStage) match {
        case Some(merged) => merged :: acc.tail
        case None => nextStage :: acc
      }
    }.reverse

  // Currently a search on the second query prevents merging, as its meaning ought to
  // be "search the output of the first query" rather than "search the underlying
  // dataset".  Unfortunately this means "select :*,*" isn't a left-identity of merge
  // for a query that contains a search.
  private def tryMerge(a: Analysis, b: Analysis): Option[Analysis] = (a, b) match {
    case (SoQLAnalysis(aIsGroup, false, aSelect, aFrom, None, aWhere, aGroup, aHaving, aOrder, aLim, aOff, aSearch),
          SoQLAnalysis(false,    false, bSelect, bFrom, bJoin, None,   None,   None,    None,   bLim, bOff, None)) if
          // Do not merge when the previous soql is grouped and the next soql has joins
          // select g, count(x) as cx group by g |> select g, cx, @b.a join @b on @b.g=g
          // Newly introduced columns from joins cannot be merged and brought in w/o grouping and aggregate functions.
          !(aIsGroup && bJoin.nonEmpty) && aFrom == bFrom =>
      // we can merge a change of only selection and limit + offset onto anything
      val (newLim, newOff) = Merger.combineLimits(aLim, aOff, bLim, bOff)
      Some(SoQLAnalysis(isGrouped = aIsGroup,
                        distinct = false,
                        selection = mergeSelection(aSelect, bSelect),
                        from = aFrom,
                        join = bJoin,
                        where = aWhere,
                        groupBy = aGroup,
                        having = aHaving,
                        orderBy = aOrder,
                        limit = newLim,
                        offset = newOff,
                        search = aSearch))
    case (SoQLAnalysis(false, false, aSelect, aFrom, None, aWhere, None, None, aOrder, None, None, aSearch),
          SoQLAnalysis(false, false, bSelect, bFrom, bJoin, bWhere, None, None, bOrder, bLim, bOff, None)) if
          aFrom == bFrom =>
      // Can merge a change of filter or order only if no window was specified on the left
      Some(SoQLAnalysis(isGrouped = false,
                        distinct = false,
                        selection = mergeSelection(aSelect, bSelect),
                        from = aFrom,
                        join = bJoin,
                        where = mergeWhereLike(aSelect, aWhere, bWhere),
                        groupBy = None,
                        having = None,
                        orderBy = mergeOrderBy(aSelect, aOrder, bOrder),
                        limit = bLim,
                        offset = bOff,
                        search = aSearch))
    case (SoQLAnalysis(false, false, aSelect, aFrom, None, aWhere, None,     None,    _,      None, None, aSearch),
          SoQLAnalysis(true, false, bSelect, bFrom, bJoin, bWhere, bGroupBy, bHaving, bOrder, bLim, bOff, None)) if
          aFrom == bFrom =>
      // an aggregate on a non-aggregate
      Some(SoQLAnalysis(isGrouped = true,
                        distinct = false,
                        selection = mergeSelection(aSelect, bSelect),
                        from = aFrom,
                        join = bJoin,
                        where = mergeWhereLike(aSelect, aWhere, bWhere),
                        groupBy = mergeGroupBy(aSelect, bGroupBy),
                        having = mergeWhereLike(aSelect, None, bHaving),
                        orderBy = mergeOrderBy(aSelect, None, bOrder),
                        limit = bLim,
                        offset = bOff,
                        search = aSearch))
    case (SoQLAnalysis(true,  false, aSelect, aFrom, None, aWhere, aGroupBy, aHaving, aOrder, None, None, aSearch),
          SoQLAnalysis(false, false, bSelect, bFrom, None, bWhere, None,     None,    bOrder, bLim, bOff, None)) if
          aFrom == bFrom =>
      // a non-aggregate on an aggregate -- merge the WHERE of the second with the HAVING of the first
      Some(SoQLAnalysis(isGrouped = true,
                        distinct = false,
                        selection = mergeSelection(aSelect, bSelect),
                        from = aFrom,
                        join = None,
                        where = aWhere,
                        groupBy = aGroupBy,
                        having = mergeWhereLike(aSelect, aHaving, bWhere),
                        orderBy = mergeOrderBy(aSelect, aOrder, bOrder),
                        limit = bLim,
                        offset = bOff,
                        search = aSearch))
    case (_, _) =>
      None
  }

  private def mergeSelection(a: OrderedMap[ColumnName, Expr],
                             b: OrderedMap[ColumnName, Expr]): OrderedMap[ColumnName, Expr] =
    // ok.  We don't need anything from A, but columnRefs in b's expr that refer to a's values need to get substituted
    b.mapValues(replaceRefs(a, _))

  private def mergeOrderBy(aliases: OrderedMap[ColumnName, Expr],
                           obA: Option[Seq[typed.OrderBy[ColumnName, T]]],
                           obB: Option[Seq[typed.OrderBy[ColumnName, T]]]): Option[Seq[typed.OrderBy[ColumnName, T]]] =
    (obA, obB) match {
      case (None, None) => None
      case (Some(a), None) => Some(a)
      case (None, Some(b)) => Some(b.map { ob => ob.copy(expression = replaceRefs(aliases, ob.expression)) })
      case (Some(a), Some(b)) => Some(b.map { ob => ob.copy(expression = replaceRefs(aliases, ob.expression)) } ++ a)
    }

  private def mergeWhereLike(aliases: OrderedMap[ColumnName, Expr],
                             a: Option[Expr],
                             b: Option[Expr]): Option[Expr] =
    (a, b) match {
      case (None, None) => None
      case (Some(a), None) => Some(a)
      case (None, Some(b)) => Some(replaceRefs(aliases, b))
      case (Some(a), Some(b)) => Some(typed.FunctionCall(andFunction, List(a, replaceRefs(aliases, b)))(NoPosition, NoPosition))
    }

  private def mergeGroupBy(aliases: OrderedMap[ColumnName, Expr],
                           gb: Option[Seq[Expr]]): Option[Seq[Expr]] =
    gb.map(_.map(replaceRefs(aliases, _)))

  private def replaceRefs(a: OrderedMap[ColumnName, Expr],
                             b: Expr): Expr =
    b match {
      case cr@typed.ColumnRef(Some(q), c, t) =>
        cr
      case cr@typed.ColumnRef(None, c, t) =>
        a.getOrElse(c, cr)
      case tl: typed.TypedLiteral[T] =>
        tl
      case fc@typed.FunctionCall(f, params) =>
        typed.FunctionCall(f, params.map(replaceRefs(a, _)))(fc.position, fc.functionNamePosition)
    }
}

private object Merger {
  def combineLimits(aLim: Option[BigInt], aOff: Option[BigInt], bLim: Option[BigInt], bOff: Option[BigInt]): (Option[BigInt], Option[BigInt]) = {
    // ok, what we're doing here is basically finding the intersection of two segments of
    // the integers, where either segment may end at infinity.  Note that all the inputs are
    // non-negative (enforced by the parser).  This simplifies things somewhat
    // because we can assume that aOff + bOff > aOff
    require(aLim.fold(true)(_ >= 0), "Negative aLim")
    require(aOff.fold(true)(_ >= 0), "Negative aOff")
    require(bLim.fold(true)(_ >= 0), "Negative bLim")
    require(bOff.fold(true)(_ >= 0), "Negative bOff")
    (aLim, aOff, bLim, bOff) match {
      case (None, None, bLim, bOff) =>
        (bLim, bOff)
      case (None, Some(aOff), bLim, bOff) =>
        // first is unbounded to the right
        (bLim, Some(aOff + bOff.getOrElse(BigInt(0))))
      case (Some(aLim), aOff, Some(bLim), bOff) =>
        // both are bound to the right
        val trueAOff = aOff.getOrElse(BigInt(0))
        val trueBOff = bOff.getOrElse(BigInt(0)) + trueAOff
        val trueAEnd = trueAOff + aLim
        val trueBEnd = trueBOff + bLim

        val trueOff = trueBOff min trueAEnd
        val trueEnd = trueBEnd min trueAEnd
        val trueLim = trueEnd - trueOff

        (Some(trueLim), Some(trueOff))
      case (Some(aLim), aOff, None, bOff) =>
        // first is bound to the right but the second is not
        val trueAOff = aOff.getOrElse(BigInt(0))
        val trueBOff = bOff.getOrElse(BigInt(0)) + trueAOff

        val trueEnd = trueAOff + aLim
        val trueOff = trueBOff min trueEnd
        val trueLim = trueEnd - trueOff
        (Some(trueLim), Some(trueOff))
    }
  }
}
