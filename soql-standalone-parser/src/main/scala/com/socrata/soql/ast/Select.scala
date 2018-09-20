package com.socrata.soql.ast

import scala.util.parsing.input.{NoPosition, Position}
import com.socrata.soql.environment._
import Select.itrToString

/**
  * A SubSelect represents (potentially chained) soql that is required to have an alias
  * (because subqueries need aliases)
  */
case class SubSelect(selects: List[Select], alias: String)

/**
  * All joins must select from another table. A join may also join on sub-select. A join on a sub-select requires an
  * alias, but the alias is optional if the join is on a table-name only (e.g. "join 4x4").
  *
  *   "join 4x4" => JoinSelect(TableName(4x4, None), None) { aliasOpt = None }
  *   "join 4x4 as a" => JoinSelect(TableName(4x4, a), None) { aliasOpt = a }
  *   "join (select id from 4x4) as a" =>
  *     JoinSelect(TableName(4x4, None), Some(SubSelect(List(_select_id_), a))) { aliasOpt = a }
  *   "join (select c.id from 4x4 as c) as a" =>
  *     JoinSelect(TableName(4x4, Some(c)), Some(SubSelect(List(_select_id_), a))) { aliasOpt = a }
  */
case class JoinSelect(fromTable: TableName, subSelect: Option[SubSelect]) {
  // The overall alias for the join select, which is the alias for the subSelect, if defined.
  // Otherwise, it is the alias for the TableName, if defined.
  val alias: Option[String] =  subSelect.map(_.alias).orElse(fromTable.alias)
  def selects: List[Select] = subSelect.map(_.selects).getOrElse(Nil)

  override def toString: String = {
    val (subSelectStr, aliasStrOpt) = subSelect.map { case SubSelect(h :: tail, subAlias) =>
      val selectWithFromStr = h.toStringWithFrom(fromTable)
      val selectStr = (selectWithFromStr :: tail.map(_.toString)).mkString("|>")
      (s"($selectStr)", Some(subAlias))
    }.getOrElse((fromTable.toString, None))

    List(subSelectStr, itrToString("AS", aliasStrOpt)).filter(_.nonEmpty).mkString(" ")
  }
}

object Select {
  def itrToString[A](prefix: String, l: Iterable[A], sep: String = " ") = {
    if (l.nonEmpty) {
      l.mkString(prefix, sep, "")
    } else {
      ""
    }
  }
}

/**
  * Represents a single select statement, not including the from. Top-level selects have an implicit "from"
  * based on the current view. Joins do require a "from" (which is a member of the JoinSelect class). A List[Select]
  * represents chained soql (e.g. "select a, b |> select a"), and is what is returned from a top-level parse of a
  * soql string (see Parser#selectStatement).
  *
  * the chained soql:
  *   "select id, a |> select a"
  * is equivalent to:
  *   "select a from (select id, a [from current view]) as alias"
  * and is represented as:
  *   List(select_id_a, select_id)
  */
case class Select(
  distinct: Boolean,
  selection: Selection,
  joins: List[Join],
  where: Option[Expression],
  groupBys: List[Expression],
  having: Option[Expression],
  orderBys: List[OrderBy],
  limit: Option[BigInt],
  offset: Option[BigInt],
  search: Option[String]) {

  private def toString(from: Option[TableName]): String = {
    if(AST.pretty) {
      val distinctStr = if (distinct) "DISTINCT " else ""
      val selectStr = s"SELECT $distinctStr$selection"
      val fromStr = from.map(_.toString).getOrElse("")
      val joinsStr = joins.mkString(" ")
      val whereStr = itrToString("WHERE", where)
      val groupByStr = itrToString("GROUP BY", groupBys)
      val havingStr = itrToString("HAVING", having)
      val obStr = itrToString("ORDER BY", orderBys, ",")
      val limitStr = itrToString("LIMIT", limit)
      val offsetStr = itrToString("OFFSET", offset)
      val searchStr = itrToString("SEARCH", search.map(Expression.escapeString))

      val parts = List(selectStr, fromStr, joinsStr, whereStr, groupByStr, havingStr, obStr, limitStr, offsetStr, searchStr)
      parts.filter(_.nonEmpty).mkString(" ")
    } else {
      AST.unpretty(this)
    }
  }

  def toStringWithFrom(fromTable: TableName): String = toString(Some(fromTable))

  override def toString: String = toString(None)
}

// represents the columns being selected. examples:
// "first_name, last_name as last"
// "*"
// "*(except id)"
// ":*"      <-- all columns including system columns (date_created, etc.)
// "sum(count) as s"
case class Selection(allSystemExcept: Option[StarSelection], allUserExcept: Seq[StarSelection], expressions: Seq[SelectedExpression]) {
  override def toString = {
    if(AST.pretty) {
      def star(s: StarSelection, token: String) = {
        val sb = new StringBuilder()
        s.qualifier.foreach { x =>
          sb.append(x.replaceFirst(TableName.SodaFountainTableNamePrefix, TableName.Prefix))
          sb.append(TableName.Field)
        }
        sb.append(token)
        if(s.exceptions.nonEmpty) {
          sb.append(s.exceptions.map(e => e._1).mkString(" (EXCEPT ", ", ", ")"))
        }
        sb.toString
      }
      (allSystemExcept.map(star(_, ":*")) ++ allUserExcept.map(star(_, "*")) ++ expressions.map(_.toString)).mkString(", ")
    } else {
      AST.unpretty(this)
    }
  }

  def isSimple = allSystemExcept.isEmpty && allUserExcept.isEmpty && expressions.isEmpty
}

case class StarSelection(qualifier: Option[String], exceptions: Seq[(ColumnName, Position)]) {
  var starPosition: Position = NoPosition
  def positionedAt(p: Position): this.type = {
    starPosition = p
    this
  }
}

case class SelectedExpression(expression: Expression, name: Option[(ColumnName, Position)]) {
  override def toString =
    if(AST.pretty) {
      name match {
        case Some(name) => expression + " AS " + name._1
        case None => expression.toString
      }
    } else {
      AST.unpretty(this)
    }
}

case class OrderBy(expression: Expression, ascending: Boolean, nullLast: Boolean) {
  override def toString =
    if(AST.pretty) {
      expression + (if(ascending) " ASC" else " DESC") + (if(nullLast) " NULL LAST" else " NULL FIRST")
    } else {
      AST.unpretty(this)
    }
}

object SimpleSelect {
  /**
    * Simple Select is a select created by a join where a sub-query is not used like "JOIN @aaaa-aaaa"
    */
  def isSimple(selects: List[Select]): Boolean = {
    selects.forall(_.selection.isSimple)
  }
}
