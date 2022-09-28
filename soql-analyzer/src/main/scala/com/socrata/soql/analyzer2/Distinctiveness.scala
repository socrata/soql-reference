package com.socrata.soql.analyzer2

import com.socrata.prettyprint.prelude._

import com.socrata.soql.collection._
import com.socrata.soql.typechecker.HasDoc

sealed trait Distinctiveness[+CT, +CV] {
  private[analyzer2] def doRewriteDatabaseNames(state: RewriteDatabaseNamesState): Distinctiveness[CT, CV]
  private[analyzer2] def doRelabel(state: RelabelState): Distinctiveness[CT, CV]
  private[analyzer2] def findIsomorphism[CT2 >: CT, CV2 >: CV](state: IsomorphismState, that: Distinctiveness[CT2, CV2]): Boolean
  private[analyzer2] def columnReferences: Map[TableLabel, Set[ColumnLabel]]
  def debugDoc(implicit ev: HasDoc[CV]): Option[Doc[Annotation[Nothing, CT]]]
}
object Distinctiveness {
  case object Indistinct extends Distinctiveness[Nothing, Nothing] {
    private[analyzer2] def doRewriteDatabaseNames(state: RewriteDatabaseNamesState) = this
    private[analyzer2] def doRelabel(state: RelabelState) = this
    private[analyzer2] def findIsomorphism[CT2, CV2](state: IsomorphismState, that: Distinctiveness[CT2, CV2]): Boolean =
      that == Indistinct
  private[analyzer2] def columnReferences: Map[TableLabel, Set[ColumnLabel]] = Map.empty

    def debugDoc(implicit ev: HasDoc[Nothing]) = None
  }

  case object FullyDistinct extends Distinctiveness[Nothing, Nothing] {
    private[analyzer2] def doRewriteDatabaseNames(state: RewriteDatabaseNamesState) = this
    private[analyzer2] def doRelabel(state: RelabelState) = this
    private[analyzer2] def findIsomorphism[CT2, CV2](state: IsomorphismState, that: Distinctiveness[CT2, CV2]): Boolean =
      that == FullyDistinct

    // Uggh.. this is a little weird (and the reason this method is
    // "private[analyzer2]") since FullyDistinct kind of references
    // all columns in the select in which it appears.
    private[analyzer2] def columnReferences: Map[TableLabel, Set[ColumnLabel]] = Map.empty

    def debugDoc(implicit ev: HasDoc[Nothing]) = Some(d"DISTINCT")
  }

  case class On[+CT, +CV](exprs: Seq[Expr[CT, CV]]) extends Distinctiveness[CT, CV] {
    private[analyzer2] def doRewriteDatabaseNames(state: RewriteDatabaseNamesState) =
      On(exprs.map(_.doRewriteDatabaseNames(state)))
    private[analyzer2] def doRelabel(state: RelabelState) =
      On(exprs.map(_.doRelabel(state)))

    private[analyzer2] def columnReferences: Map[TableLabel, Set[ColumnLabel]] =
      exprs.foldLeft(Map.empty[TableLabel, Set[ColumnLabel]]) { (acc, e) =>
        acc.mergeWith(e.columnReferences)(_ ++ _)
      }

    private[analyzer2] def findIsomorphism[CT2 >: CT, CV2 >: CV](state: IsomorphismState, that: Distinctiveness[CT2, CV2]): Boolean =
      that match {
        case On(thatExprs) =>
          this.exprs.length == thatExprs.length &&
            this.exprs.zip(thatExprs).forall { case (a, b) => a.findIsomorphism(state, b) }
        case _ => false
      }

    def debugDoc(implicit ev: HasDoc[CV]) =
      Some(exprs.map(_.debugDoc).encloseNesting(d"DISTINCT ON (", d",", d")"))
  }
}