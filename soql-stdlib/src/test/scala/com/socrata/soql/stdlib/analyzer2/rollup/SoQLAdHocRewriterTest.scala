package com.socrata.soql.stdlib.analyzer2.rollup

import java.math.{BigDecimal => JBigDecimal}

import org.scalatest.{FunSuite, MustMatchers}

import com.socrata.soql.analyzer2._
import com.socrata.soql.analyzer2.mocktablefinder._
import com.socrata.soql.environment.Provenance
import com.socrata.soql.functions.{SoQLFunctions, MonomorphicFunction, SoQLTypeInfo, SoQLFunctionInfo}
import com.socrata.soql.types._

class SoQLAdHocRewriterTest extends FunSuite with MustMatchers {
  import SoQLTypeInfo.hasType

  trait MT extends MetaTypes {
    type ColumnType = SoQLType
    type ColumnValue = SoQLValue
    type ResourceNameScope = Int
    type DatabaseTableNameImpl = String
    type DatabaseColumnNameImpl = String
  }

  val rewriter = new SoQLAdHocRewriter[MT]

  def analyze(expr: String) = {
    val tf = MockTableFinder[MT](
      (0, "rollup") -> D("floating_timestamp" -> SoQLFloatingTimestamp)
    )
    val q = s"select $expr from @rollup"
    val Right(ft) = tf.findTables(0, q, Map.empty)
    val analyzer = new SoQLAnalyzer[MT](
      SoQLTypeInfo.soqlTypeInfo2(numericRowIdLiterals = false),
      SoQLFunctionInfo,
      new ToProvenance[String] {
        override def toProvenance(dtn: DatabaseTableName[String]) = Provenance(dtn.name)
      })
    val Right(analysis) = analyzer(ft, UserParameters.empty)
    analysis.statement.asInstanceOf[Select[MT]].selectList.valuesIterator.next().expr
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
}
