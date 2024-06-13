package com.socrata.soql.sqlizer

import com.socrata.prettyprint.prelude._

import com.socrata.soql.functions.Function

object TestFunctionSqlizer extends FuncallSqlizer[TestHelper.TestMT] {
  override val exprSqlFactory = TestHelper.TestExprSqlFactory

  def compress(f: FunctionCall, args: Seq[ExprSql], ctx: DynamicContext) = {
    assert(f.function.minArity == 1 && !f.function.isVariadic)
    assert(args.lengthCompare(1) == 0)
    args.head.compressed
  }

  def sqlizeExpandedFunction(f: FunctionCall, args: Seq[ExprSql], ctx: DynamicContext) = {
    assert(args.isEmpty)
    exprSqlFactory(Seq(Doc("\"column a\""), Doc("\"column b\"")), f)
  }

  val ordinaryFunctionMap = Map[Function[CT], (FunctionCall, Seq[ExprSql], DynamicContext) => ExprSql](
    TestFunctions.Eq -> sqlizeEq _,
    TestFunctions.Plus -> sqlizeBinaryOp("+"),
    TestFunctions.Times -> sqlizeBinaryOp("*"),
    TestFunctions.Compress -> compress,
    TestFunctions.NonTrivialFunctionWhichProducesAnExpandedCompoundValue -> sqlizeExpandedFunction
  )

  override def sqlizeOrdinaryFunction(e: FunctionCall, args: Seq[ExprSql], ctx: DynamicContext): ExprSql =
    ordinaryFunctionMap(e.function.function)(e, args, ctx)

  override def sqlizeAggregateFunction(e: AggregateFunctionCall, args: Seq[ExprSql], filter: Option[ExprSql], ctx: DynamicContext): ExprSql = ???

  val windowedFunctionMap = Map[Function[CT], (WindowedFunctionCall, Seq[ExprSql], Option[ExprSql], Seq[ExprSql], Seq[OrderBySql], DynamicContext) => ExprSql](
    TestFunctions.WindowFunction -> sqlizeNormalWindowedFuncall("windowed_function")
  )

  def sqlizeWindowedFunction(e: WindowedFunctionCall, args: Seq[ExprSql], filter: Option[ExprSql], partitionBy: Seq[ExprSql], orderBy: Seq[OrderBySql], ctx: DynamicContext): ExprSql =
    windowedFunctionMap(e.function.function)(e, args, filter, partitionBy, orderBy, ctx)

}
