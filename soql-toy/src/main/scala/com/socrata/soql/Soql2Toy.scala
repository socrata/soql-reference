package com.socrata.soql.analyzer2

import com.socrata.soql.types._
import com.socrata.soql.environment.{ColumnName, DatasetContext, TableName, HoleName, Provenance}
import com.socrata.soql.functions.{SoQLFunctionInfo, SoQLTypeInfo, SoQLFunctions, MonomorphicFunction}
import com.rojoma.json.v3.util.JsonUtil
import com.socrata.soql.parsing.{Parser, AbstractParser}
import scala.io.StdIn.readLine
import com.socrata.soql.analyzer2._
import com.socrata.soql.analyzer2.mocktablefinder._
import com.socrata.soql.types.obfuscation.CryptProvider
import com.socrata.soql.stdlib.analyzer2.SoQLRewritePassHelpers

trait Soql2Toy extends MetaTypes {
  type ResourceNameScope = Int
  type ColumnType = SoQLType
  type ColumnValue = SoQLValue
  type DatabaseTableNameImpl = String
  type DatabaseColumnNameImpl = String
}

object Soql2Toy extends (Array[String] => Unit) with StatementUniverse[Soql2Toy] {
  def fail(msg: String) = {
    System.err.println(msg)
    sys.exit(1)
  }

  val tf = MockTableFinder[Soql2Toy](
    (0, "ds") -> D(
      ":id" -> SoQLID,
      ":created_at" -> SoQLFixedTimestamp,
      ":updated_at" -> SoQLFloatingTimestamp,
      ":version" -> SoQLVersion,
      "name_last" -> SoQLText,
      "name_first" -> SoQLText,
      "visits" -> SoQLNumber,
      "last_visit" -> SoQLFixedTimestamp,
      "address" -> SoQLLocation,
      "balance" -> SoQLNumber
    ).withPrimaryKey(":id"),
    (0, "udf") -> U(0, "select * from @ds where last_visit > ?a", "a" -> SoQLFixedTimestamp)
  )

  val rewritePasses = Seq(
    rewrite.Pass.Merge
  )

  val provenanceMapper = new ToProvenance with FromProvenance {
    def fromProvenance(dtn: Provenance): DatabaseTableName = DatabaseTableName(dtn.value)
    def toProvenance(dtn: DatabaseTableName): Provenance = Provenance(dtn.name)
  }

  val rewritePassHelpers = new SoQLRewritePassHelpers[Soql2Toy]

  def apply(args: Array[String]): Unit = {
    implicit object hasDoc extends HasDoc[CV] {
      def docOf(cv: CV) = cv.doc(CryptProvider.zeros)
    }

    val analyzer = new SoQLAnalyzer[Soql2Toy](SoQLTypeInfo.soqlTypeInfo2(numericRowIdLiterals = false), SoQLFunctionInfo, provenanceMapper).
      preserveSystemColumns { (columnName, expr) =>
        if(columnName == ColumnName(":id")) {
          Some(AggregateFunctionCall[Soql2Toy](MonomorphicFunction(SoQLFunctions.Max, Map("a" -> SoQLID)), Seq(expr), false, None)(FuncallPositionInfo.Synthetic))
        } else {
          None
        }
      }

    while(true) {
      val selection = readLine("> ")
      if(selection == null) return;
      else if(selection == "exit" || selection == "quit") {
        return
      } else {
        tf.findTables(0, selection, Map.empty) match {
          case Right(ft) =>
            analyzer(ft, UserParameters.empty) match {
              case Right(unrewrittenAnalysis) =>
                val analysis = unrewrittenAnalysis.applyPasses(rewritePasses, rewritePassHelpers)

                println(analysis.statement.debugStr)
                val longestName =
                  if(analysis.statement.schema.isEmpty) {
                    0
                  } else {
                    analysis.statement.schema.valuesIterator.map(_.name.name.length).max
                  }
                for(ent <- analysis.statement.schema.values) {
                  print(ent.name.name + " " + "." * (longestName - ent.name.name.length) + ".. " + ent.typ)
                  if(ent.isSynthetic) {
                    print(" (synthetic)")
                  }
                  println()
                }
              case Left(err) =>
                println(err)
            }
          case Left(err) =>
            println(err)
        }
      }
    }
  }
}
