package com.socrata.soql.analyzer2

import scala.util.parsing.input.NoPosition

import org.scalatest.{FunSuite, MustMatchers}

import com.socrata.soql.collection._

import mocktablefinder._

class SoQLAnalysisTest extends FunSuite with MustMatchers with TestHelper {
  val rowNumber = TestFunctions.RowNumber.monomorphic.get
  val windowFunction = TestFunctions.WindowFunction.monomorphic.get
  val and = TestFunctions.And.monomorphic.get

  test("direct query does not add a column") {
    val tf = tableFinder(
      (0, "twocol") -> D("text" -> TestText, "num" -> TestNumber)
    )

    val analysis = analyze(tf, "twocol", "select * order by text")
    analysis.preserveOrdering.statement.schema.size must equal (2)
  }

  test("unordered-on-unordered doesn't change under order preservation") {
    val tf = tableFinder(
      (0, "twocol") -> D("text" -> TestText, "num" -> TestNumber)
    )

    val analysis = analyze(tf, "twocol", "select * |> select *")
    analysis.preserveOrdering.statement must equal (analysis.statement)
  }

  test("unordered-on-ordered pipes does not generate an ordering column if unnecessary") {
    val tf = tableFinder(
      (0, "twocol") -> D("text" -> TestText, "num" -> TestNumber)
    )

    val analysis = analyze(tf, "twocol", "select * order by num |> select *")
    val expectedAnalysis = analyze(tf, "twocol", "select * order by num |> select * order by num")
    analysis.preserveOrdering.statement must be (isomorphicTo(expectedAnalysis.statement))
  }

  test("unordered-on-ordered pipes generates an ordering column if necessary") {
    val tf = tableFinder(
      (0, "twocol") -> D("text" -> TestText, "num" -> TestNumber)
    )

    val analysis = analyze(tf, "twocol", "select * order by num+1 |> select *")
    val expectedAnalysis = analyze(tf, "twocol", "select *, num+1 as ordering order by num+1 |> select * (except ordering) order by ordering")
    analysis.preserveOrdering.statement must be (isomorphicTo(expectedAnalysis.statement))
  }

  test("ordered-on-ordered pipes does not generate an ordering column if unnecessary") {
    val tf = tableFinder(
      (0, "twocol") -> D("text" -> TestText, "num" -> TestNumber)
    )

    val analysis = analyze(tf, "twocol", "select * order by num |> select * order by text")
    val expectedAnalysis = analyze(tf, "twocol", "select * order by num |> select * order by text, num")
    analysis.preserveOrdering.statement must be (isomorphicTo(expectedAnalysis.statement))
  }

  test("ordered-on-ordered pipes generates an ordering column") {
    val tf = tableFinder(
      (0, "twocol") -> D("text" -> TestText, "num" -> TestNumber)
    )

    val analysis = analyze(tf, "twocol", "select * order by num+1 |> select * order by text")
    val expectedAnalysis = analyze(tf, "twocol", "select *, num+1 as ordering order by num+1 |> select * (except ordering) order by text, ordering")
    analysis.preserveOrdering.statement must be (isomorphicTo(expectedAnalysis.statement))
  }

  test("orderings are blocked by aggregations and do not continue beyond them if not required by a window function") {
    val tf = tableFinder(
      (0, "twocol") -> D("text" -> TestText, "num" -> TestNumber)
    )

    val analysis = analyze(tf, "twocol", """
select * order by num+1
  |> select *
  |> select *
  |> select text, num group by text, num order by num+2, text+'x'
  |> select *
""")

    val expectedAnalysis = analyze(tf, "twocol", """
select * order by num+1
  |> select *
  |> select *
  |> select text, num, num+2 as o1, text+'x' as o2 group by text, num order by num+2, text+'x'
  |> select * (except o1, o2) order by o1, o2
""")

    analysis.preserveOrdering.statement must be (isomorphicTo(expectedAnalysis.statement))
  }

  test("orderings are blocked by aggregations but continue again beyond them if required") {
    val tf = tableFinder(
      (0, "twocol") -> D("text" -> TestText, "num" -> TestNumber)
    )

    val analysis = analyze(tf, "twocol", """
select * order by num+1
  |> select *
  |> select *, window_function() over ()
  |> select text, num group by text, num order by num+2, text+'x'
  |> select *
""")

    val expectedAnalysis = analyze(tf, "twocol", """
select *, num+1 as ordering order by num+1
  |> select * order by ordering
  |> select * (except ordering), window_function() over () order by ordering
  |> select text, num, num+2 as o1, text+'x' as o2 group by text, num order by num+2, text+'x'
  |> select * (except o1, o2) order by o1, o2
""")

    analysis.preserveOrdering.statement must be (isomorphicTo(expectedAnalysis.statement))
  }

  test("simple merge") {
    val tf = tableFinder(
      (0, "twocol") -> D("text" -> TestText, "num" -> TestNumber)
    )

    val analysis = analyze(tf, "twocol", """
select * order by num limit 20 offset 10 |> select text + text as t2, num * 2 as num offset 10
""")

    val expectedAnalysis = analyze(tf, "twocol", """
select text + text, num * 2 as num from @this as t order by @t.num limit 10 offset 20
""")

    analysis.merge(and).statement must be (isomorphicTo(expectedAnalysis.statement))
  }

  test("merge - distinct on ordered by different column") {
    val tf = tableFinder(
      (0, "twocol") -> D("text" -> TestText, "num" -> TestNumber)
    )

    val analysis = analyze(tf, "twocol", """
select text, num order by num |> select distinct text
""")

    val expectedAnalysis = analyze(tf, "twocol", """
select distinct text
""")

    analysis.merge(and).statement must be (isomorphicTo(expectedAnalysis.statement))
  }

  test("merge - distinct on ordered by same column") {
    val tf = tableFinder(
      (0, "twocol") -> D("text" -> TestText, "num" -> TestNumber)
    )

    val analysis = analyze(tf, "twocol", """
select text, num order by text |> select distinct text
""")

    val expectedAnalysis = analyze(tf, "twocol", """
select distinct text order by text
""")

    analysis.merge(and).statement must be (isomorphicTo(expectedAnalysis.statement))
  }

  test("merge - unsimple distinct on ordered") {
    val tf = tableFinder(
      (0, "twocol") -> D("text" -> TestText, "num" -> TestNumber)
    )

    val analysis = analyze(tf, "twocol", """
select text, num order by num |> select distinct text, num order by text
""")

    val expectedAnalysis = analyze(tf, "twocol", """
select distinct text, num order by text, num
""")

    analysis.merge(and).statement must be (isomorphicTo(expectedAnalysis.statement))
  }

  test("merge - distinct aggregate on ordered") {
    val tf = tableFinder(
      (0, "twocol") -> D("text" -> TestText, "num" -> TestNumber)
    )

    val analysis = analyze(tf, "twocol", """
select text, num order by num |> select distinct text group by text
""")

    val expectedAnalysis = analyze(tf, "twocol", """
select distinct text group by text
""")

    analysis.merge(and).statement must be (isomorphicTo(expectedAnalysis.statement))
  }

  test("merge - aggregate on non-aggregate") {
    val tf = tableFinder(
      (0, "twocol") -> D("text" -> TestText, "num" -> TestNumber)
    )

    val analysis = analyze(tf, "twocol", """
select text, num where num = 3 order by num |> select text, count(*) group by text
""")

    val expectedAnalysis = analyze(tf, "twocol", """
select text, count(*) where num = 3 group by text
""")

    analysis.merge(and).statement must be (isomorphicTo(expectedAnalysis.statement))
  }

  test("merge - non-aggregate on aggregate") {
    val tf = tableFinder(
      (0, "twocol") -> D("text" -> TestText, "num" -> TestNumber)
    )

    val analysis = analyze(tf, "twocol", """
select text, count(num) group by text |> select * where count_num = 5
""")

    val expectedAnalysis = analyze(tf, "twocol", """
select text, count(num) as n group by text having n = 5
""")

    analysis.merge(and).statement must be (isomorphicTo(expectedAnalysis.statement))
  }

  test("merge - simple on windowed") {
    val tf = tableFinder(
      (0, "twocol") -> D("text" -> TestText, "num" -> TestNumber)
    )

    val analysis = analyze(tf, "twocol", """
select text, row_number() over () as rn |> select text, rn + 1 limit 5
""")

    val expectedAnalysis = analyze(tf, "twocol", """
select text, row_number() over () + 1 limit 5
""")

    analysis.merge(and).statement must be (isomorphicTo(expectedAnalysis.statement))
  }

  test("merge - filter on join") {
    val tf = tableFinder(
      (0, "twocol") -> D("text" -> TestText, "num" -> TestNumber),
      (0, "locowt") -> D("amount" -> TestNumber, "words" -> TestText)
    )

    val analysis = analyze(tf, "twocol", """
select text, num, @ct.amount, @ct.words join @locowt as @ct on num = @ct.amount |> select * where amount = 3
""")

    val expectedAnalysis = analyze(tf, "twocol", """
select text, num, @ct.amount, @ct.words join @locowt as @ct on num = @ct.amount where @ct.amount = 3
""")

    analysis.merge(and).statement must be (isomorphicTo(expectedAnalysis.statement))
  }

  test("merge - join on simple") {
    val tf = tableFinder(
      (0, "twocol") -> D("text" -> TestText, "num" -> TestNumber),
      (0, "locowt") -> D("amount" -> TestNumber, "words" -> TestText)
    )

    val analysis = analyze(tf, "twocol", """
select text, num order by num |> select * join @locowt as ct on num = @ct.amount
""")

    val expectedAnalysis = analyze(tf, "twocol", """
select text, num join @locowt as ct on num = @ct.amount order by num
""")

    analysis.merge(and).statement must be (isomorphicTo(expectedAnalysis.statement))
  }

  test("merge - implicit group-by") {
    val tf = tableFinder(
      (0, "twocol") -> D("text" -> TestText, "num" -> TestNumber)
    )

    val analysis = analyze(tf, "twocol", """
select count(*), 1 as x |> select x
""")

    val expectedAnalysis = analyze(tf, "twocol", """
select count(*), 1 as x |> select x
""")

    analysis.merge(and).statement must be (isomorphicTo(expectedAnalysis.statement))
  }

  test("remove unused columns - simple") {
    val tf = tableFinder(
      (0, "twocol") -> D("text" -> TestText, "num" -> TestNumber)
    )

    val analysis = analyze(tf, "twocol", """
select * order by num |> select text
""")

    val expectedAnalysis = analyze(tf, "twocol", """
select text order by num |> select text
""")

    analysis.removeUnusedColumns.statement must be (isomorphicTo(expectedAnalysis.statement))
  }

  test("remove unused columns - preserve implicit group") {
    val tf = tableFinder(
      (0, "twocol") -> D("text" -> TestText, "num" -> TestNumber)
    )

    val analysis = analyze(tf, "twocol", """
select count(*), 1 as x |> select x
""")

    val expectedAnalysis = analyze(tf, "twocol", """
select count(*), 1 as x |> select x
""")

    analysis.removeUnusedColumns.statement must be (isomorphicTo(expectedAnalysis.statement))
  }

  test("preserve system columns - simple") {
    val tf = tableFinder(
      (0, "twocol") -> D(":id" -> TestNumber, "text" -> TestText, "num" -> TestNumber)
    )

    val analysis = analyze(tf, "twocol", "select text + text, num * 2")

    val expectedAnalysis = analyze(tf, "twocol", "select text + text, num * 2, :id")

    analysis.preserveSystemColumns(_ => None).statement must be (isomorphicTo(expectedAnalysis.statement))
  }

  test("preserve system columns - partially selected") {
    val tf = tableFinder(
      (0, "twocol") -> D(":id" -> TestNumber, ":version" -> TestNumber, "text" -> TestText, "num" -> TestNumber)
    )

    val analysis = analyze(tf, "twocol", "select :id, text + text, num * 2")

    val expectedAnalysis = analyze(tf, "twocol", "select :id, text + text, num * 2, :version")

    analysis.preserveSystemColumns(_ => None).statement must be (isomorphicTo(expectedAnalysis.statement))
  }

  test("preserve system columns - distinct blocks") {
    val tf = tableFinder(
      (0, "twocol") -> D(":id" -> TestNumber, ":version" -> TestNumber, "text" -> TestText, "num" -> TestNumber)
    )

    val analysis = analyze(tf, "twocol", "select distinct text + text, num * 2")

    val expectedAnalysis = analyze(tf, "twocol", "select distinct text + text, num * 2")

    analysis.preserveSystemColumns(_ => None).statement must be (isomorphicTo(expectedAnalysis.statement))
  }

  test("preserve system columns - aggregating") {
    val tf = tableFinder(
      (0, "twocol") -> D(":id" -> TestNumber, "text" -> TestText, "num" -> TestNumber)
    )

    val analysis = analyze(tf, "twocol", "select text, count(*) group by text")

    val expectedAnalysis = analyze(tf, "twocol", "select text, count(*), max(:id) group by text")

    def numberMerge(e: Expr[TestType, TestValue]) =
      e.typ match {
        case TestNumber =>
          Some(
            AggregateFunctionCall(
              TestFunctions.Max.monomorphic.get,
              Seq(e),
              false,
              None
            )(FuncallPositionInfo.None)
          )
        case _ => None
      }

    analysis.preserveSystemColumns(numberMerge).statement must be (isomorphicTo(expectedAnalysis.statement))
    analysis.preserveSystemColumns(numberMerge).statement.schema.find(_._2.name == cn(":id")) must not be empty
  }

  test("simple (de)serialization") {
    val tf = tableFinder(
      (0, "twocol") -> D("text" -> TestText, "num" -> TestNumber),
      (0, "names") -> D("num" -> TestNumber, "first" -> TestText, "last" -> TestText),
      (0, "join") -> Q(0, "names", "SELECT first, last, @twocol.text JOIN @twocol ON num = @twocol.num")
    )

    val analysis = analyze(tf, "join", """
select * where first = 'Tom'
""")

    implicit val mfDeser = com.socrata.soql.functions.MonomorphicFunction.deserialize(TestFunctionInfo)
    val deser = serialization.ReadBuffer.read[SoQLAnalysis[Int, TestType, TestValue]](serialization.WriteBuffer.asBytes(analysis))
    deser.statement must equal (analysis.statement)
  }
}
