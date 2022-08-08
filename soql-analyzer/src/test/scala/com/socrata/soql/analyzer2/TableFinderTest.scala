package com.socrata.soql.analyzer2

import org.scalatest.FunSuite
import org.scalatest.MustMatchers

import com.socrata.soql.BinaryTree
import com.socrata.soql.ast.Select
import com.socrata.soql.collection.OrderedMap
import com.socrata.soql.environment.{ColumnName, ResourceName}
import com.socrata.soql.parsing.standalone_exceptions.LexerParserException
import com.socrata.soql.parsing.{StandaloneParser, AbstractParser}

import mocktablefinder._

class TableFinderTest extends FunSuite with MustMatchers {
  val tables = new MockTableFinder(
    Map(
      (0, "t1") -> D(Map(
        "key" -> "integer",
        "value" -> "thing"
      )),
      (0, "t2") -> D(Map(
        "key" -> "integer",
        "value" -> "otherthing"
      )),
      (0, "t3") -> Q(0, "t2", "select *"),
      (0, "t4") -> U(0, "select * from @t2", OrderedMap.empty),
      (0, "t5") -> Q(1, "t1", "select *"),
      (0, "t6") -> Q(1, "t2", "select *"), // t2 exists in scope 0 but not in scope 1
      (1, "t1") -> D(Map())
    )
  )

  test("can find a table") {
    tables.findTables(0, "select * from @t1").map(_.tableMap) must equal (tables((0, "t1")))
  }

  test("can fail to find a table") {
    tables.findTables(0, "select * from @doesnt-exist").map(_.tableMap) must equal (tables.notFound(0, "doesnt-exist"))
  }

  test("can find a table implicitly") {
    tables.findTables(0, ResourceName("t1"), "select key, value").map(_.tableMap) must equal (tables((0, "t1")))
  }

  test("can find a joined table") {
    tables.findTables(0, ResourceName("t1"), "select key, value join @t2 on @t2.key = key").map(_.tableMap) must equal (tables((0, "t1"), (0, "t2")))
  }

  test("can find a query that accesses another table") {
    tables.findTables(0, ResourceName("t1"), "select key, value join @t3 on @t3.key = key").map(_.tableMap) must equal (tables((0, "t1"), (0, "t2"), (0, "t3")))
  }

  test("can find a query that accesses another table via udf") {
    tables.findTables(0, ResourceName("t1"), "select key, value join @t4(1,2,3) on @t4.key = key").map(_.tableMap) must equal (tables((0, "t1"), (0, "t2"), (0, "t4")))
  }

  test("can find a in a different scope") {
    tables.findTables(0, ResourceName("t1"), "select key, value join @t5 on @t5.key = key").map(_.tableMap) must equal (tables((0, "t1"), (0, "t5"), (1, "t1")))
  }

  test("can fail to find a query in a different scope") {
    tables.findTables(0, ResourceName("t1"), "select key, value join @t6 on @t6.key = key").map(_.tableMap) must equal (tables.notFound(1, "t2"))
  }
}
