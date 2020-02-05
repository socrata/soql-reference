package com.socrata.soql

import scala.util.parsing.input.{NoPosition, Position}
import java.io.{ByteArrayOutputStream, OutputStream}

import com.google.protobuf.CodedOutputStream
import gnu.trove.impl.Constants
import gnu.trove.map.hash.TObjectIntHashMap
import com.socrata.soql.parsing.SoQLPosition
import com.socrata.soql.typed._
import com.socrata.soql.functions.MonomorphicFunction
import com.socrata.soql.collection.{OrderedMap, NonEmptySeq}
import com.socrata.soql.environment.{ResourceName, ColumnName, Qualified, TableRef}
import com.socrata.soql.ast.{JoinType, InnerJoinType, LeftOuterJoinType, RightOuterJoinType, FullOuterJoinType}

private trait SerializationDictionary[T] {
  def registerType(typ: T): Int
  def registerString(s: String): Int
  def registerResourceName(rn: ResourceName): Int
  def registerColumnName(rn: ColumnName): Int
  def registerFunction(func: MonomorphicFunction[T]): Int
}

class AnalysisSerializer[T](serializeType: T => String) extends ((OutputStream, NonEmptySeq[SoQLAnalysis[Qualified[ColumnName], T]]) => Unit) {
  type Expr = CoreExpr[Qualified[ColumnName], T]
  type Order = OrderBy[Qualified[ColumnName], T]

  private class SerializationDictionaryImpl extends SerializationDictionary[T] {
    private def makeMap[A] = new TObjectIntHashMap[A](Constants.DEFAULT_CAPACITY, Constants.DEFAULT_LOAD_FACTOR, -1)
    private def register[A](map: TObjectIntHashMap[A], a: A): Int = {
      val count = map.size()
      map.putIfAbsent(a, count) match {
        case -1 => count
        case old => old
      }
    }

    val strings = makeMap[String] // This MUST be written BEFORE types, resourceNames, columnNames and functions!
    val types = makeMap[T] // This MUST be written BEFORE functions!
    val resourceNames = makeMap[ResourceName]
    val columnNames = makeMap[ColumnName]
    val functions = makeMap[MonomorphicFunction[T]]

    def registerString(s: String): Int =
      register(strings, s)

    def registerType(typ: T): Int =
      types.get(typ) match {
        case -1 =>
          val id = registerString(serializeType(typ))
          types.put(typ, id)
          id
        case id =>
          id
      }

    def registerResourceName(rn: ResourceName): Int =
      resourceNames.get(rn) match {
        case -1 =>
          val id = registerString(rn.name)
          resourceNames.put(rn, id)
          id
        case id =>
          id
      }

    def registerColumnName(col: ColumnName): Int =
      columnNames.get(col) match {
        case -1 =>
          val id = registerString(col.name)
          columnNames.put(col, id)
          id
        case id =>
          id
      }

    def registerFunction(func: MonomorphicFunction[T]): Int = {
      val count = functions.size
      functions.putIfAbsent(func, count) match {
        case -1 =>
          registerString(func.function.identity)
          func.bindings.foreach { case (typeVar, typ) =>
            registerString(typeVar)
            registerType(typ)
          }
          count
        case id =>
          id
      }
    }

    private def saveRegistry[A](out: CodedOutputStream, registry: TObjectIntHashMap[A])(f: A => Unit) {
      out.writeUInt32NoTag(registry.size)
      val it = registry.iterator
      while(it.hasNext) {
        it.advance()
        f(it.key)
        out.writeUInt32NoTag(it.value)
      }
    }

    private def saveFunctions(out: CodedOutputStream) =
      saveRegistry(out, functions) { case MonomorphicFunction(function, bindings) =>
        out.writeUInt32NoTag(strings.get(function.identity))
        out.writeUInt32NoTag(bindings.size)
        for((typeVar, typ) <- bindings) {
          out.writeUInt32NoTag(strings.get(typeVar))
          out.writeUInt32NoTag(types.get(typ))
        }
      }

    private def saveStrings(out: CodedOutputStream) =
      saveRegistry(out, strings) { s =>
        out.writeStringNoTag(s)
      }

    private def saveSimpleRegistry[A](out: CodedOutputStream, registry: TObjectIntHashMap[A]) {
      out.writeUInt32NoTag(registry.size)
      val it = registry.iterator
      while(it.hasNext) {
        it.advance()
        out.writeUInt32NoTag(it.value)
      }
    }

    private def saveTypes(out: CodedOutputStream) =
      saveSimpleRegistry(out, types)

    private def saveResourceNames(out: CodedOutputStream) =
      saveSimpleRegistry(out, resourceNames)

    private def saveColumnNames(out: CodedOutputStream) =
      saveSimpleRegistry(out, columnNames)

    def save(out: CodedOutputStream) {
      saveStrings(out)
      saveResourceNames(out)
      saveColumnNames(out)
      saveTypes(out)
      saveFunctions(out)
    }
  }

  private class Serializer(out: CodedOutputStream, dictionary: SerializationDictionary[T]) {
    import dictionary._

    private def writePosition(pos: Position) {
      pos match {
        case SoQLPosition(line, column, sourceText, offset) =>
          out.writeRawByte(0)
          out.writeUInt32NoTag(line)
          out.writeUInt32NoTag(column)
          out.writeUInt32NoTag(dictionary.registerString(sourceText))
          out.writeUInt32NoTag(offset)
        case NoPosition =>
          out.writeRawByte(1)
        case other =>
          out.writeRawByte(2)
          out.writeUInt32NoTag(pos.line)
          out.writeUInt32NoTag(pos.column)
          // the position doesn't expose the raw line value *sigh*...
          // We'll just have to hope longString hasn't been overridden.
          val ls = pos.longString
          val newlinePos = ls.indexOf('\n')
          val line = if(newlinePos == -1) "" else ls.substring(0, newlinePos)
          out.writeUInt32NoTag(dictionary.registerString(line))
      }
    }

    private def writeColumnName(col: Qualified[ColumnName]) {
      val Qualified(qualifier, colName) = col
      writeTableRef(qualifier)
      out.writeUInt32NoTag(dictionary.registerColumnName(colName))
    }

    private def writeExpr(e: Expr) {
      writePosition(e.position)
      e match {
        case ColumnRef(col, typ) =>
          out.writeRawByte(1)
          writeColumnName(col)
          out.writeUInt32NoTag(registerType(typ))
        case StringLiteral(value, typ) =>
          out.writeRawByte(2)
          out.writeUInt32NoTag(registerString(value))
          out.writeUInt32NoTag(registerType(typ))
        case NumberLiteral(value, typ) =>
          out.writeRawByte(3)
          out.writeUInt32NoTag(registerString(value.toString))
          out.writeUInt32NoTag(registerType(typ))
        case BooleanLiteral(value, typ) =>
          out.writeRawByte(4)
          out.writeBoolNoTag(value)
          out.writeUInt32NoTag(registerType(typ))
        case NullLiteral(typ) =>
          out.writeRawByte(5)
          out.writeUInt32NoTag(registerType(typ))
        case f@FunctionCall(func, params) =>
          out.writeRawByte(6)
          writePosition(f.functionNamePosition)
          out.writeUInt32NoTag(registerFunction(func))
          writeSeq(params)(writeExpr)
      }
    }

    private def writeGrouped(isGrouped: Boolean) =
      out.writeBoolNoTag(isGrouped)

    private def writeDistinct(distinct: Boolean) =
      out.writeBoolNoTag(distinct)

    private def writeSelection(selection: OrderedMap[ColumnName, Expr]) {
      writeSeq(selection) { case (col, expr) =>
        out.writeUInt32NoTag(dictionary.registerColumnName(col))
        writeExpr(expr)
      }
    }

    private def writeJoinType(joinType: JoinType) {
      val n =
        joinType match {
          case InnerJoinType => 1
          case LeftOuterJoinType => 2
          case RightOuterJoinType => 3
          case FullOuterJoinType => 4
        }
      out.writeRawByte(n)
    }

    private def writeJoins(joins: Seq[Join[Qualified[ColumnName], T]]) {
      writeSeq(joins) { join =>
        writeJoinType(join.typ)
        writeJoinAnalysis(join.from)
        writeExpr(join.on)
      }
    }

    private def writeJoinAnalysis(ja: JoinAnalysis[Qualified[ColumnName], T]): Unit = {
      val JoinAnalysis(TableRef.Primary(from), analyses, TableRef.Join(to)) = ja
      out.writeUInt32NoTag(dictionary.registerResourceName(from))
      writeSeq(analyses)(writeAnalysis)
      out.writeUInt32NoTag(to)
    }

    private def writeTableRef(tn: TableRef) = {
      tn match {
        case TableRef.Primary(name) =>
          out.writeRawByte(1)
          out.writeUInt32NoTag(dictionary.registerResourceName(name))
        case TableRef.PreviousChainStep =>
          out.writeRawByte(2)
        case TableRef.Join(num) =>
          out.writeRawByte(3)
          out.writeUInt32NoTag(num)
      }
    }

    def writeSeq[A](list: Iterable[A])(f: A => Unit): Unit = {
      out.writeUInt32NoTag(list.size)
      list.foreach(f)
    }

    private def maybeWrite[A](x: Option[A])(f: A => Unit): Unit = x match {
      case Some(a) =>
        out.writeBoolNoTag(true)
        f(a)
      case None =>
        out.writeBoolNoTag(false)
    }

    private def writeWhere(where: Option[Expr]) =
      maybeWrite(where) { writeExpr }

    private def writeGroupBy(groupBy: Seq[Expr]) =
      writeSeq(groupBy)(writeExpr)

    private def writeHaving(expr: Option[Expr]) =
      writeWhere(expr)

    private def writeSingleOrderBy(orderBy: Order) = {
      val OrderBy(expr, ascending, nullsLast) = orderBy
      writeExpr(expr)
      out.writeBoolNoTag(ascending)
      out.writeBoolNoTag(nullsLast)
    }

    private def writeOrderBy(orderBy: Seq[Order]) =
      writeSeq(orderBy)(writeSingleOrderBy)

    private def writeLimit(limit: Option[BigInt]) =
      maybeWrite(limit) { n =>
        out.writeUInt32NoTag(registerString(n.toString))
      }

    private def writeOffset(offset: Option[BigInt]) =
      writeLimit(offset)

    private def writeSearch(search: Option[String]) =
      maybeWrite(search) { s =>
        out.writeUInt32NoTag(registerString(s))
      }

    def writeAnalysis(analysis: SoQLAnalysis[Qualified[ColumnName], T]) {
      val SoQLAnalysis(isGrouped,
                       distinct,
                       selection,
                       join,
                       where,
                       groupBy,
                       having,
                       orderBy,
                       limit,
                       offset,
                       search) = analysis
      writeGrouped(isGrouped)
      writeDistinct(analysis.distinct)
      writeSelection(selection)
      writeJoins(join)
      writeWhere(where)
      writeGroupBy(groupBy)
      writeHaving(having)
      writeOrderBy(orderBy)
      writeLimit(limit)
      writeOffset(offset)
      writeSearch(search)
    }

    def write(analyses: NonEmptySeq[SoQLAnalysis[Qualified[ColumnName], T]]): Unit = {
      writeSeq(analyses.seq)(writeAnalysis)
    }
  }

  def apply(outputStream: OutputStream, analyses: NonEmptySeq[SoQLAnalysis[Qualified[ColumnName], T]]) {
    val dictionary = new SerializationDictionaryImpl
    val postDictionaryData = new ByteArrayOutputStream
    val out = CodedOutputStream.newInstance(postDictionaryData)
    val serializer = new Serializer(out, dictionary)
    serializer.write(analyses)
    out.flush()

    val codedOutputStream = CodedOutputStream.newInstance(outputStream)
    codedOutputStream.writeInt32NoTag(AnalysisDeserializer.CurrentVersion) // version number
    dictionary.save(codedOutputStream)
    codedOutputStream.flush()
    postDictionaryData.writeTo(outputStream)
  }
}
