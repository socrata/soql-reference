package com.socrata.soql.analyzer2

import com.socrata.soql.environment.ColumnName
import com.socrata.soql.analyzer2.serialization.{Readable, ReadBuffer, Writable, WriteBuffer}

case class NamedExpr[MT <: MetaTypes](expr: Expr[MT], name: ColumnName) extends LabelHelper[MT] {
  private[analyzer2] def doRewriteDatabaseNames(state: RewriteDatabaseNamesState) =
    this.copy(expr = expr.doRewriteDatabaseNames(state))

  private[analyzer2] def doRelabel(state: RelabelState) =
    copy(expr = expr.doRelabel(state))
}

object NamedExpr {
  implicit def serialize[MT <: MetaTypes](implicit ev: Writable[Expr[MT]]) = new Writable[NamedExpr[MT]] {
    def writeTo(buffer: WriteBuffer, ne: NamedExpr[MT]): Unit = {
      buffer.write(ne.expr)
      buffer.write(ne.name)
    }
  }

  implicit def deserialize[MT <: MetaTypes](implicit ev: Readable[Expr[MT]]) = new Readable[NamedExpr[MT]] {
    def readFrom(buffer: ReadBuffer): NamedExpr[MT] = {
      NamedExpr(
        expr = buffer.read[Expr[MT]](),
        name = buffer.read[ColumnName]()
      )
    }
  }
}
