package com.socrata.soql.analyzer2

import scala.language.higherKinds
import scala.util.parsing.input.Position

import com.socrata.prettyprint.prelude._
import com.socrata.prettyprint.Pretty

import com.socrata.soql.collection._
import com.socrata.soql.functions.MonomorphicFunction
import com.socrata.soql.typechecker.{FunctionInfo, HasType, HasDoc}
import com.socrata.soql.analyzer2.serialization.{Readable, ReadBuffer, Writable, WriteBuffer}

import DocUtils._

sealed abstract class Expr[+CT, +CV] extends Product {
  type Self[+CT, +CV] <: Expr[CT, CV]

  val typ: CT
  val position: Position

  val size: Int

  def isAggregated: Boolean
  def isWindowed: Boolean

  private[analyzer2] def doRewriteDatabaseNames(expr: RewriteDatabaseNamesState): Self[CT, CV]

  private[analyzer2] def doRelabel(state: RelabelState): Self[CT, CV]

  private[analyzer2] def reposition(p: Position): Self[CT, CV]

  private[analyzer2] def findIsomorphism[CT2 >: CT, CV2 >: CV](state: IsomorphismState, that: Expr[CT2, CV2]): Boolean
  private[analyzer2] def columnReferences: Map[TableLabel, Set[ColumnLabel]]

  final def debugStr(implicit ev: HasDoc[CV]): String = debugStr(new StringBuilder).toString
  final def debugStr(sb: StringBuilder)(implicit ev: HasDoc[CV]): StringBuilder = debugDoc.layoutSmart().toStringBuilder(sb)
  final def debugDoc(implicit ev: HasDoc[CV]): Doc[Annotation[Nothing, CT]] =
    doDebugDoc.annotate(Annotation.Typed(typ))
  protected def doDebugDoc(implicit ev: HasDoc[CV]): Doc[Annotation[Nothing, CT]]

  def find(predicate: Expr[CT, CV] => Boolean): Option[Expr[CT, CV]]

  final def contains[CT2 >: CT, CV2 >: CV](e: Expr[CT2, CV2]): Boolean = find(_ == e).isDefined

  // Since this is completely immutable, cache the hashCode rather
  // than recomputing, as these trees can be quite deep.  Annoying
  // that it has to be lazy, but otherwise this gets initialized too
  // early.
  override final lazy val hashCode: Int = scala.runtime.ScalaRunTime._hashCode(this)
}
object Expr {
  implicit def serialize[CT: Writable, CV: Writable]: Writable[Expr[CT, CV]] = new Writable[Expr[CT, CV]] {
    implicit val self = this
    def writeTo(buffer: WriteBuffer, t: Expr[CT, CV]): Unit =
      t match {
        case c : Column[CT] =>
          buffer.write(0)
          buffer.write(c)
        case slr: SelectListReference[CT] =>
          buffer.write(1)
          buffer.write(slr)
        case lv: LiteralValue[CT, CV] =>
          buffer.write(2)
          buffer.write(lv)
        case nl: NullLiteral[CT] =>
          buffer.write(3)
          buffer.write(nl)
        case fc: FunctionCall[CT, CV] =>
          buffer.write(4)
          buffer.write(fc)
        case afc: AggregateFunctionCall[CT, CV] =>
          buffer.write(5)
          buffer.write(afc)
        case wfc: WindowedFunctionCall[CT, CV] =>
          buffer.write(6)
          buffer.write(wfc)
      }
  }

  implicit def deserialize[CT: Readable, CV: Readable](implicit hasType: HasType[CV, CT], functionInfo: Readable[MonomorphicFunction[CT]]): Readable[Expr[CT, CV]] = new Readable[Expr[CT, CV]] {
    implicit val self = this
    def readFrom(buffer: ReadBuffer): Expr[CT, CV] =
      buffer.read[Int]() match {
        case 0 => buffer.read[Column[CT]]()
        case 1 => buffer.read[SelectListReference[CT]]()
        case 2 => buffer.read[LiteralValue[CT, CV]]()
        case 3 => buffer.read[NullLiteral[CT]]()
        case 4 => buffer.read[FunctionCall[CT, CV]]()
        case 5 => buffer.read[AggregateFunctionCall[CT, CV]]()
        case 6 => buffer.read[WindowedFunctionCall[CT, CV]]()
        case other => fail("Unknown expression tag " + other)
      }
  }
}

/********* Column *********/

final case class Column[+CT](
  table: TableLabel,
  column: ColumnLabel,
  typ: CT
)(
  val position: Position
) extends
    Expr[CT, Nothing]
    with expression.ColumnImpl[CT]
object Column extends expression.OColumnImpl

/********* Select list reference *********/

final case class SelectListReference[+CT](
  index: Int,
  isAggregated: Boolean,
  isWindowed: Boolean,
  typ: CT
)(
  val position: Position
) extends
    Expr[CT, Nothing]
    with expression.SelectListReferenceImpl[CT]
object SelectListReference extends expression.OSelectListReferenceImpl

/********* Literal *********/

sealed abstract class Literal[+CT, +CV]
    extends Expr[CT, CV]
    with expression.LiteralImpl[CT, CV]

/********* Literal value *********/

final case class LiteralValue[+CT, +CV](
  value: CV
)(
  val position: Position
)(
  implicit ev: HasType[CV, CT]
) extends
    Literal[CT, CV]
    with expression.LiteralValueImpl[CT, CV]
{
  // these need to be here and not in the impl for variance reasons
  val typ = ev.typeOf(value)
  private[analyzer2] def reposition(p: Position): Self[CT, CV] = copy()(position = p)
}
object LiteralValue extends expression.OLiteralValueImpl

/********* Null literal *********/

final case class NullLiteral[+CT](
  typ: CT
)(
  val position: Position
) extends
    Literal[CT, Nothing]
    with expression.NullLiteralImpl[CT]
object NullLiteral extends expression.ONullLiteralImpl

/********* FuncallLike *********/

sealed abstract class FuncallLike[+CT, +CV] extends Expr[CT, CV] with Product

/********* Function call *********/

final case class FunctionCall[+CT, +CV](
  function: MonomorphicFunction[CT],
  args: Seq[Expr[CT, CV]]
)(
  val position: Position,
  val functionNamePosition: Position
) extends
    FuncallLike[CT, CV]
    with expression.FunctionCallImpl[CT, CV]
object FunctionCall extends expression.OFunctionCallImpl

/********* Aggregate function call *********/

final case class AggregateFunctionCall[+CT, +CV](
  function: MonomorphicFunction[CT],
  args: Seq[Expr[CT, CV]],
  distinct: Boolean,
  filter: Option[Expr[CT, CV]]
)(
  val position: Position,
  val functionNamePosition: Position
) extends
    FuncallLike[CT, CV]
    with expression.AggregateFunctionCallImpl[CT, CV]
{
  require(function.isAggregate)
}
object AggregateFunctionCall extends expression.OAggregateFunctionCallImpl

/********* Windowed function call *********/

final case class WindowedFunctionCall[+CT, +CV](
  function: MonomorphicFunction[CT],
  args: Seq[Expr[CT, CV]],
  filter: Option[Expr[CT, CV]],
  partitionBy: Seq[Expr[CT, CV]], // is normal right here, or should it be aggregate?
  orderBy: Seq[OrderBy[CT, CV]], // ditto thus
  frame: Option[Frame]
)(
  val position: Position,
  val functionNamePosition: Position
) extends
    FuncallLike[CT, CV]
    with expression.WindowedFunctionCallImpl[CT, CV]
{
  require(function.needsWindow || function.isAggregate)
}

object WindowedFunctionCall extends expression.OWindowedFunctionCallImpl
