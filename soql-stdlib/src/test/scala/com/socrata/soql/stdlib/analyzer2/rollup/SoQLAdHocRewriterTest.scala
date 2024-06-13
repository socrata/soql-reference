package com.socrata.soql.stdlib.analyzer2.rollup

import java.math.{BigDecimal => JBigDecimal}

import org.scalatest.{FunSuite, MustMatchers}

import com.socrata.soql.analyzer2._
import com.socrata.soql.analyzer2.mocktablefinder._
import com.socrata.soql.environment.Provenance
import com.socrata.soql.functions.{SoQLFunctions, MonomorphicFunction, SoQLTypeInfo, SoQLFunctionInfo}
import com.socrata.soql.types._

class SoQLAdHocRewriterTest extends FunSuite with MustMatchers with SoQLRollupTestHelper {
  import SoQLTypeInfo.hasType

  val rewriter = new SoQLAdHocRewriter[MT]

  def analyze(expr: String) = {
    val tf = MockTableFinder[MT](
      (0, "rollup") -> D("floating_timestamp" -> SoQLFloatingTimestamp, "fixed_timestamp" -> SoQLFixedTimestamp)
    )
    val q = s"select $expr from @rollup"
    val Right(ft) = tf.findTables(0, q, Map.empty)
    val analyzer = new SoQLAnalyzer[MT](
      SoQLTypeInfo.soqlTypeInfo2(numericRowIdLiterals = false),
      SoQLFunctionInfo,
      new ToProvenance {
        override def toProvenance(dtn: DatabaseTableName) = Provenance(dtn.name)
      })
    val Right(analysis) = analyzer(ft, UserParameters.empty)
    analysis.statement.asInstanceOf[Select].selectList.valuesIterator.next().expr
  }

  private val c1 =
    PhysicalColumn[MT](
      AutoTableLabel.forTest(1),
      DatabaseTableName("rollup"),
      DatabaseColumnName("c1"),
      SoQLNumber
    )(AtomicPositionInfo.Synthetic)

  test("limited by literals's truncatedness") {
    rewriter(analyze("floating_timestamp < '2001-01-02'")) must equal (Seq(analyze("date_trunc_ymd(floating_timestamp) < '2001-01-02'")))
    rewriter(analyze("floating_timestamp < '2001-02-01'")) must equal (Seq(analyze("date_trunc_ymd(floating_timestamp) < '2001-02-01'"), analyze("date_trunc_ym(floating_timestamp) < '2001-02-01'")))
    rewriter(analyze("floating_timestamp < '2001-01-01'")) must equal (Seq(analyze("date_trunc_ymd(floating_timestamp) < '2001-01-01'"), analyze("date_trunc_ym(floating_timestamp) < '2001-01-01'"), analyze("date_trunc_y(floating_timestamp) < '2001-01-01'")))

    rewriter(analyze("floating_timestamp >= '2001-01-02'")) must equal (Seq(analyze("date_trunc_ymd(floating_timestamp) >= '2001-01-02'")))
    rewriter(analyze("floating_timestamp >= '2001-02-01'")) must equal (Seq(analyze("date_trunc_ymd(floating_timestamp) >= '2001-02-01'"), analyze("date_trunc_ym(floating_timestamp) >= '2001-02-01'")))
    rewriter(analyze("floating_timestamp >= '2001-01-01'")) must equal (Seq(analyze("date_trunc_ymd(floating_timestamp) >= '2001-01-01'"), analyze("date_trunc_ym(floating_timestamp) >= '2001-01-01'"), analyze("date_trunc_y(floating_timestamp) >= '2001-01-01'")))
  }

  test("limited by expr's truncatedness") {
    rewriter(analyze("date_trunc_ymd(floating_timestamp) < '2001-01-01'")) must equal (Seq(analyze("date_trunc_ym(floating_timestamp) < '2001-01-01'"), analyze("date_trunc_y(floating_timestamp) < '2001-01-01'")))
    rewriter(analyze("date_trunc_ym(floating_timestamp) < '2001-01-01'")) must equal (Seq(analyze("date_trunc_y(floating_timestamp) < '2001-01-01'")))

    rewriter(analyze("date_trunc_ymd(floating_timestamp) >= '2001-01-01'")) must equal (Seq(analyze("date_trunc_ym(floating_timestamp) >= '2001-01-01'"), analyze("date_trunc_y(floating_timestamp) >= '2001-01-01'")))
    rewriter(analyze("date_trunc_ym(floating_timestamp) >= '2001-01-01'")) must equal (Seq(analyze("date_trunc_y(floating_timestamp) >= '2001-01-01'")))
  }

  test("limited by both") {
    rewriter(analyze("date_trunc_ymd(floating_timestamp) < '2001-02-01'")) must equal (Seq(analyze("date_trunc_ym(floating_timestamp) < '2001-02-01'")))
  }

  test("trunc at zone") {
    rewriter(analyze("date_trunc_ymd(fixed_timestamp, 'zone me!')")) must equal (Seq(analyze("date_trunc_ymd(to_floating_timestamp(fixed_timestamp, 'zone me!'))")))
    rewriter(analyze("date_trunc_ym(fixed_timestamp, 'zone me!')")) must equal (Seq(analyze("date_trunc_ym(to_floating_timestamp(fixed_timestamp, 'zone me!'))")))
    rewriter(analyze("date_trunc_y(fixed_timestamp, 'zone me!')")) must equal (Seq(analyze("date_trunc_y(to_floating_timestamp(fixed_timestamp, 'zone me!'))")))
  }

  test("trunc of converted timestamp") {
    rewriter(analyze("date_trunc_ymd(to_floating_timestamp(fixed_timestamp, 'zone me!'))")) must equal (Seq(analyze("date_trunc_ymd(fixed_timestamp, 'zone me!')")))
    rewriter(analyze("date_trunc_ym(to_floating_timestamp(fixed_timestamp, 'zone me!'))")) must equal (Seq(analyze("date_trunc_ym(fixed_timestamp, 'zone me!')")))
    rewriter(analyze("date_trunc_y(to_floating_timestamp(fixed_timestamp, 'zone me!'))")) must equal (Seq(analyze("date_trunc_y(fixed_timestamp, 'zone me!')")))
  }

  test("timestamp trunc does not loop") {
    val tf = MockTableFinder[MT](
      (0, "dataset") -> D("fixed_timestamp" -> SoQLFixedTimestamp, "number" -> SoQLNumber),
      (1, "rollup") -> D("c1" -> SoQLFloatingTimestamp, "c2" -> SoQLNumber).withPrimaryKey("c1")
    )

    val analysis = analyze(tf, "select date_trunc_y(fixed_timestamp, 'US/Pacific') as y, sum(number) from @dataset group by y")
    val rollup = SoQLRollupInfo(1, "rollup", tf, "select date_trunc_ymd(to_floating_timestamp(fixed_timestamp, 'US/Pacific')) as y, sum(number) from @dataset group by y")
    val Some(result) = rollupExact(analysis.statement.asInstanceOf[Select], rollup, analysis.labelProvider)

    locally {
      val Right(expectedRollupFT) = tf.findTables(1, "select date_trunc_y(c1) as y, sum(c2) from @rollup group by y", Map.empty)
      val Right(expectedRollupAnalysis) = analyzer(expectedRollupFT, UserParameters.empty)
      result must be (isomorphicTo(expectedRollupAnalysis.statement))
    }
  }
}
