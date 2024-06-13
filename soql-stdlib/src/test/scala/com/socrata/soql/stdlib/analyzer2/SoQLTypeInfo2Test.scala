package com.socrata.soql.stdlib.analyzer2

import java.math.{BigDecimal => JBigDecimal}

import org.scalatest.{FunSuite, MustMatchers}

import com.socrata.soql.analyzer2._
import com.socrata.soql.analyzer2.{UserParameters => AnalyzerUserParameters}
import com.socrata.soql.analyzer2.mocktablefinder._
import com.socrata.soql.environment.{Provenance, TypeName}
import com.socrata.soql.functions.{SoQLFunctions, MonomorphicFunction, SoQLTypeInfo, SoQLFunctionInfo}
import com.socrata.soql.types.{SoQLType, SoQLValue, SoQLNumber, SoQLText, SoQLID}

class SoQLTypeInfo2Test extends FunSuite with MustMatchers {
  import SoQLTypeInfo.hasType

  trait MT extends MetaTypes {
    type ColumnType = SoQLType
    type ColumnValue = SoQLValue
    type ResourceNameScope = Int
    type DatabaseTableNameImpl = String
    type DatabaseColumnNameImpl = String
  }

  def analyze(expr: String, numericRowIdLiterals: Boolean) = {
    val tf = MockTableFinder[MT](
      (0, "ds") -> D(":id" -> SoQLID)
    )
    val q = s"select $expr from @ds"
    val Right(ft) = tf.findTables(0, q, Map.empty)
    val analyzer = new SoQLAnalyzer[MT](
      SoQLTypeInfo.soqlTypeInfo2(numericRowIdLiterals),
      SoQLFunctionInfo,
      new ToProvenance[String] {
        override def toProvenance(dtn: DatabaseTableName[String]) = Provenance(dtn.name)
      })
    analyzer(ft, AnalyzerUserParameters.empty).map { analysis =>
      analysis.statement.asInstanceOf[Select[MT]].selectList.valuesIterator.next().expr
    }
  }

  test("numeric row id literals accepted when numeric row id literals are on") {
    analyze(":id = 5", numericRowIdLiterals = true) match {
      case Right(
        FunctionCall(
          MonomorphicFunction(SoQLFunctions.Eq, bindings),
          Seq(PhysicalColumn(_, _, _, SoQLID), LiteralValue(id : SoQLID))
        )
      ) =>
        bindings must be (Map("a" -> SoQLID))
        id.value must be (5)
        id.provenance must be (Some(Provenance("ds")))
      case Right(_) =>
        fail("was not column = literal")
      case Left(e) =>
        fail("should have accepted")
    }
  }

  test("fractional numeric row id literals accepted when numeric row id literals are on") {
    analyze(":id = 5.5", numericRowIdLiterals = true) match {
      case Left(SoQLAnalyzerError.TypecheckError.TypeMismatch(_, _, SoQLNumber.name)) =>
        // ok
      case Left(_) =>
        fail("wrong error")
      case Right(_) =>
        fail("should not have accepted")
    }
  }

  test("numeric-string row id literals accepted when numeric row id literals are on") {
    analyze(":id = '5'", numericRowIdLiterals = true) match {
      case Right(
        FunctionCall(
          MonomorphicFunction(SoQLFunctions.Eq, bindings),
          Seq(PhysicalColumn(_, _, _, SoQLID), LiteralValue(id : SoQLID))
        )
      ) =>
        bindings must be (Map("a" -> SoQLID))
        id.value must be (5)
        id.provenance must be (Some(Provenance("ds")))
      case Right(_) =>
        fail("was not column = literal")
      case Left(_) =>
        fail("should have accepted")
    }
  }

  test("fractional numeric-string row id literals accepted when numeric row id literals are on") {
    analyze(":id = '5.5'", numericRowIdLiterals = true) match {
      case Left(SoQLAnalyzerError.TypecheckError.TypeMismatch(_, _, SoQLText.name)) =>
        // ok
      case Left(_) =>
        fail("wrong error")
      case Right(_) =>
        fail("should not have accepted")
    }
  }

  test("formatted row id literals rejected when numeric row id literals are on") {
    analyze(":id = 'row-2222-2222-2222'", numericRowIdLiterals = true) match {
      case Left(SoQLAnalyzerError.TypecheckError.TypeMismatch(_, _, SoQLText.name)) =>
        // ok
      case Left(_) =>
        fail("wrong error")
      case Right(_) =>
        fail("should not have accepted")
    }
  }

  test("numeric row id literals rejected when numeric row id literals are off") {
    analyze(":id = 5", numericRowIdLiterals = false) match {
      case Left(SoQLAnalyzerError.TypecheckError.TypeMismatch(_, _, SoQLNumber.name)) =>
        // ok
      case Left(_) =>
        fail("wrong error")
      case Right(_) =>
        fail("should not have been accepted")
    }
  }

  test("numeric-string row id literals not accepted when numeric row id literals are on") {
    analyze(":id = '5'", numericRowIdLiterals = false) match {
      case Left(SoQLAnalyzerError.TypecheckError.TypeMismatch(_, _, SoQLText.name)) =>
        // ok
      case Left(_) =>
        fail("wrong error")
      case Right(_) =>
        fail("should not have been accepted")
    }
  }

  test("formatted row id literals accepted when numeric row id literals are on") {
    analyze(":id = 'row-2227-2222-2222'", numericRowIdLiterals = false) match {
      case Right(
        FunctionCall(
          MonomorphicFunction(SoQLFunctions.Eq, bindings),
          Seq(PhysicalColumn(_, _, _, SoQLID), LiteralValue(id : SoQLID))
        )
      ) =>
        bindings must be (Map("a" -> SoQLID))
        id.value must be (5)
        id.provenance must be (Some(Provenance("ds")))
      case Right(_) =>
        fail("was not column = literal")
      case Left(_) =>
        fail("should have accepted")
    }
  }
}
