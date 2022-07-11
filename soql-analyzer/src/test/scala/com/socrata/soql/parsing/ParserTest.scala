package com.socrata.soql.parsing

import scala.util.parsing.input.NoPosition
import org.scalatest._
import org.scalatest.MustMatchers
import com.socrata.soql.ast._
import com.socrata.soql.exceptions.BadParse
import com.socrata.soql.environment.{ColumnName, FunctionName, HoleName}

class ParserTest extends WordSpec with MustMatchers {
  def parseExpression(soql: String) = new Parser().expression(soql)
  def parseParamExpression(soql: String) = new Parser(AbstractParser.Parameters(allowParamSpecialForms = true)).expression(soql)

  def parseFull(soql: String) = new Parser().unchainedSelectStatement(soql)

  def expectFailure(expectedMsg: String, soql: String) =
    try {
      new Parser().expression(soql)
      fail("Unexpected success")
    } catch {
      case e: BadParse => e.message must equal (expectedMsg)
    }

  def ident(name: String) = ColumnOrAliasRef(None, ColumnName(name))(NoPosition)
  def functionCall(name: FunctionName, args: Seq[Expression], filter: Option[Expression], window: Option[WindowFunctionInfo]) = FunctionCall(name, args, filter, window)(NoPosition, NoPosition)
  def stringLiteral(s: String) = StringLiteral(s)(NoPosition)
  def numberLiteral(num: BigDecimal) = NumberLiteral(num)(NoPosition)
  def booleanLiteral(bool: Boolean) = BooleanLiteral(bool)(NoPosition)
  def nullLiteral = NullLiteral()(NoPosition)
  def param(name: String, qualifier: String) = Hole.SavedQuery(HoleName(name), qualifier)(NoPosition)

  "Parsing" should {
    "require a full `between' clause" in {
      expectFailure("Expected an expression, but got end of input", "x between")
      expectFailure("Expected `AND', but got end of input", "x between a")
      expectFailure("Expected an expression, but got end of input", "x between a and")
      expectFailure("Expected one of `BETWEEN', `IN', or `LIKE', but got end of input", "x not")
      expectFailure("Expected an expression, but got end of input", "x not between")
      expectFailure("Expected `AND', but got end of input", "x not between a")
    }

    "require a full `is null' clause" in {
      expectFailure("Expected one of `NULL' or `NOT', but got end of input", "x is")
      expectFailure("Expected `NULL', but got end of input", "x is not")
      expectFailure("Expected `NULL', but got `5'", "x is not 5")
      expectFailure("Expected one of `NULL' or `NOT', but got `5'", "x is 5")
    }

    "accept both the 'case' sugar and the 'case' function" in {
      parseExpression("case when x then y end") must equal (functionCall(SpecialFunctions.Case, Seq(ident("x"), ident("y"), booleanLiteral(false), nullLiteral), None, None))
      parseExpression("case(x, y)") must equal (functionCall(FunctionName("case"), Seq(ident("x"), ident("y")), None, None))
    }

    "accept 'case' sugar with multiple clauses and an else" in {
      parseExpression("case when a then b when c then d else e end") must equal (functionCall(SpecialFunctions.Case, Seq(ident("a"), ident("b"), ident("c"), ident("d"), booleanLiteral(true), ident("e")), None, None))
    }

    "require an expression after `not'" in {
      expectFailure("Expected an expression, but got end of input", "not")
    }

    "reject a more-than-complete expression" in {
      expectFailure("Expected end of input, but got `y'", "x y")
    }

    "reject a null expression" in {
      expectFailure("Expected an expression, but got end of input", "")
    }

    "accept a lone identifier" in {
      parseExpression("a") must equal (ident("a"))
    }

    "require something after a dereference-dot" in {
      expectFailure("Expected an identifier, but got end of input", "a.")
    }

    "accept expr.identifier" in {
      parseExpression("a.b") must equal (functionCall(SpecialFunctions.Subscript, Seq(ident("a"), stringLiteral("b")), None, None))
    }

    "reject expr.identifier." in {
      expectFailure("Expected an identifier, but got end of input", "a.b.")
    }

    "reject expr[" in {
      expectFailure("Expected an expression, but got end of input", "a[")
    }

    "reject expr[expr" in {
      expectFailure("Expected `]', but got end of input", "a[2 * b")
    }

    "accept expr[expr]" in {
      parseExpression("a[2 * b]") must equal (
        functionCall(
          SpecialFunctions.Subscript,
          Seq(
            ident("a"),
            functionCall(
              SpecialFunctions.Operator("*"),
              Seq(
                numberLiteral(2),
                ident("b")), None, None)), None, None))
    }

    "accept unreserved keywords as identifiers" in {
      parseExpression("last") must equal (
        ident("last")
      )
    }

    "reject expr[expr]." in {
      expectFailure("Expected an identifier, but got end of input", "a[2 * b].")
    }

    "accept expr[expr].ident" in {
      parseExpression("a[2 * b].c") must equal (functionCall(SpecialFunctions.Subscript, Seq(
        functionCall(
          SpecialFunctions.Subscript,
          Seq(
            ident("a"),
            functionCall(SpecialFunctions.Operator("*"), Seq(
              numberLiteral(2),
              ident("b")), None, None)), None, None),
        stringLiteral("c")), None, None))
    }

    "accept expr[expr].ident[expr]" in {
      parseExpression("a[2 * b].c[3]") must equal (functionCall(SpecialFunctions.Subscript, Seq(
        functionCall(
          SpecialFunctions.Subscript,
          Seq(
            functionCall(
              SpecialFunctions.Subscript,
              Seq(
                ident("a"),
                functionCall(SpecialFunctions.Operator("*"), Seq(
                  numberLiteral(2),
                  ident("b")), None, None)), None, None),
            stringLiteral("c")), None, None),
        numberLiteral(3)), None, None))
    }

    "allow offset/limit" in {
      val x = parseFull("select * offset 6 limit 5")
      x.offset must be (Some(BigInt(6)))
      x.limit must be (Some(BigInt(5)))
    }

    "allow limit/offset" in {
      val x = parseFull("select * limit 6 offset 5")
      x.limit must be (Some(BigInt(6)))
      x.offset must be (Some(BigInt(5)))
    }

    "allow only limit" in {
      val x = parseFull("select * limit 32")
      x.limit must be (Some(BigInt(32)))
      x.offset must be (None)
    }

    "allow only offset" in {
      val x = parseFull("select * offset 7")
      x.limit must be (None)
      x.offset must be (Some(BigInt(7)))
    }

    "allow search" in {
      val x = parseFull("select * search 'WEather'")
      x.search.get must be ("WEather")
    }

    "big decimal keeps precision" in {
      val largeNumber =
        "10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002"
      val x = parseFull(s"select $largeNumber")
      x.selection.expressions.head.expression.toString must be (largeNumber)
    }

    "accept the parameter syntax" in {
      parseParamExpression("""param(@aaaa-aaaa, "a")""") must equal (param("a", "aaaa-aaaa"))
    }

  // def show[T](x: => T) {
  //   try {
  //     println(x)
  //   } catch {
  //     case l: LexerError =>
  //       val p = l.position
  //       println("[" + p.line + "." + p.column + "] failure: "+ l.getClass.getSimpleName)
  //       println()
  //       println(p.longString)
  //   }
  // }

  //   show(new LexerReader("").toStream.force)
  //   show(new LexerReader("- -1*a(1,b)*(3)==2 or x between 1 and 3").toStream.force)
  //   show(p.expression("- -1*a(1,b)*(3)==2 or x not between 1 and 3"))
  //   show(p.expression("x between a is null and a between 5 and 6 or f(x x"))
  //   show(p.expression("x x"))

  //   LEXER TESTS
  //   show(p.expression("étäøîn"))
  //   show(p.expression("_abc + -- hello world!\n123 -- gnu"))
  //   show(p.expression("\"gnu\""))
  //   show(p.expression("\"\\U01D000\""))
  //   show(p.selectStatement("SELECT x AS `where`, y AS `hither-thither`")) // this one's only a semi-lexer-test
  //   show(p.limit("12e1"))
  //   show(p.expression("`hello world`"))
  //   show(p.identifier("__gnu__"))

  //   show(p.expression("* 8"))
  //   show(p.orderings("a,b desc,c + d/e"))
  //   show(p.orderings(""))
  //   show(p.orderings("a,"))
  //   show(p.orderings("a ascdesc"))
  //   show(p.selection(""))
  //   show(p.selection("a,b,c,"))
  //   show(p.selection("a,b as,c"))
  //   show(p.selection("a,b as x,c"))
  //   show(p.selection("a,b as x,c as y"))
  //   show(p.selection("a,b as x,c as"))
  //   show(p.selectStatement(""))
  //   show(p.selectStatement("x"))
  //   show(p.selectStatement("select"))
  //   show(p.selectStatement("select a, b where"))
  //   show(p.selectStatement("select a, b group"))
  //   show(p.selectStatement("select a, b order"))
  //   show(p.selectStatement("select a, b having"))
  //   show(p.selectStatement("select * where x group by y having z order by w"))
  //   show(p.selectStatement("select *"))
  //   show(p.selectStatement("select *("))
  //   show(p.selectStatement("select *(except"))
  //   show(p.selectStatement("select *(except a"))
  //   show(p.selectStatement("select *(except a)"))
  //   show(p.selectStatement("select *(except a,"))
  //   show(p.selectStatement("select *(except a,b)"))
  //   show(p.orderings("x, y asc null last"))
  //   show(p.selectStatement("select * order by x, y gnu,"))
  //   show(p.expression("-a :: b :: c"))
  //   show(p.selectStatement("select * order by x, y limit 5"))
  //   show(p.expression("a(1) || 'blah'"))
  //   show(p.selectStatement("select x"))

  //   show(p.expression("count(__gnu__)+a+b").asInstanceOf[Parsers#Success[Expression]].result.toSyntheticIdentifierBase)
  //   show(p.expression("count(a) == 'hello, world!  This is a smiling gnu.'").asInstanceOf[Parsers#Success[Expression]].result.toSyntheticIdentifierBase)
  //   show(p.expression("count(a) == `hello-world`").asInstanceOf[Parsers#Success[Expression]].result.toSyntheticIdentifierBase)
  //   show(p.expression("`-world` + 2").asInstanceOf[Parsers#Success[Expression]].result.toSyntheticIdentifierBase)
  //   show(p.expression("world is not null").asInstanceOf[Parsers#Success[Expression]].result.toSyntheticIdentifierBase)
  //   show(p.expression("1 + `-world` = `a-` - 1").asInstanceOf[Parsers#Success[Expression]].result.toSyntheticIdentifierBase)
  //   show(p.expression(":id - 1").asInstanceOf[Parsers#Success[Expression]].result.toSyntheticIdentifierBase)
  //   show(p.expression("count(a) == 'hello, world!  This is a smiling gnu.'"))
  //   show(p.selectStatement("select x as :x"))

  //   show(p.selectStatement("select :*"))
  //   show(p.selectStatement("select *"))
  //   show(p.selectStatement("select :*,"))
  //   show(p.selectStatement("select :*,a"))
  //   show(p.selectStatement("select *,"))
  //   show(p.selectStatement("select *,a"))
  //   show(p.selectStatement("select :*,*"))
  //   show(p.selectStatement("select :*,*,"))
  //   show(p.selectStatement("select :*,*,a"))
  //   show(p.selectStatement("select :*(except a)"))
  //   show(p.selectStatement("select :*(except :a)"))
  //   show(p.selectStatement("select *(except :a)"))
  //   show(p.selectStatement("select *(except a)"))
  }
}
