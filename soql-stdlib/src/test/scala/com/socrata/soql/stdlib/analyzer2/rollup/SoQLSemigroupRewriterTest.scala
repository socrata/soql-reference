package com.socrata.soql.stdlib.analyzer2.rollup

import java.math.{BigDecimal => JBigDecimal}

import org.scalatest.{FunSuite, MustMatchers}

import com.socrata.soql.analyzer2._
import com.socrata.soql.analyzer2.mocktablefinder._
import com.socrata.soql.environment.Provenance
import com.socrata.soql.functions.{SoQLFunctions, MonomorphicFunction, SoQLTypeInfo, SoQLFunctionInfo}
import com.socrata.soql.types.{SoQLType, SoQLValue, SoQLNumber}

class SoQLSemigroupRewriterTest extends FunSuite with MustMatchers {
  import SoQLTypeInfo.hasType

  trait MT extends MetaTypes {
    type ColumnType = SoQLType
    type ColumnValue = SoQLValue
    type ResourceNameScope = Int
    type DatabaseTableNameImpl = String
    type DatabaseColumnNameImpl = String
  }

  val rewriter = new SoQLSemigroupRewriter[MT]

  def analyze(expr: String) = {
    val tf = MockTableFinder[MT](
      (0, "rollup") -> D("c1" -> SoQLNumber)
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

  test("merge max") {
    val Some(f) = rewriter(MonomorphicFunction(SoQLFunctions.Max, Map("a" -> SoQLNumber)))
    f(c1) must equal (analyze("max(c1)"))
  }

  test("merge min") {
    val Some(f) = rewriter(MonomorphicFunction(SoQLFunctions.Min, Map("a" -> SoQLNumber)))
    f(c1) must equal (analyze("min(c1)"))
  }

  test("merge sum") {
    val Some(f) = rewriter(MonomorphicFunction(SoQLFunctions.Sum, Map("a" -> SoQLNumber)))
    f(c1) must equal (analyze("sum(c1)"))
  }

  test("merge count") {
    val Some(f) = rewriter(MonomorphicFunction(SoQLFunctions.Count, Map("a" -> SoQLNumber)))
    f(c1) must equal (analyze("coalesce(sum(c1), 0)"))
  }

  test("merge count(*)") {
    val Some(f) = rewriter(SoQLFunctions.CountStar.monomorphic.get)
    f(c1) must equal (analyze("coalesce(sum(c1), 0)"))
  }
}
