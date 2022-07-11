package com.socrata.soql.parsing

import scala.collection.immutable.ListSet
import scala.collection.compat.immutable.LazyList
import scala.util.parsing.input.Position
import scala.annotation.tailrec

import com.socrata.NonEmptySeq
import com.socrata.soql.ast._
import com.socrata.soql.tokens._
import com.socrata.soql.{BinaryTree, Compound, Leaf, PipeQuery, ast, tokens}
import com.socrata.soql.environment.{ColumnName, FunctionName, HoleName, TableName, TypeName}

// Things are "protected" if they are reasonably useful to potential
// subclasses _in themselves_.  In particular, this excludes the
// various things that are pre-committed by having their first token
// consumed by the things that call them.
//
// Things which are protected in the class but not abstract should be
// `final` without a good reason otherwise.

object RecursiveDescentParser {
  final class Keyword(s: String) {
    def apply() = Identifier(s, false)
    def unapply(ident: Identifier): Boolean = {
      !ident.quoted && ident.value.equalsIgnoreCase(s)
    }
  }

  sealed abstract class Expectation {
    def printable: String
  }
  case class ActualToken(token: Token) extends Expectation {
    def printable = token.quotedPrintable
  }
  case object AnIntegerLiteral extends Expectation {
    def printable = "an integer"
  }
  case object AStringLiteral extends Expectation {
    def printable = "a string"
  }
  case object AnIdentifier extends Expectation {
    // This is used both where we expect only a user identifier and
    // where we expect either a user or a system identifier.  If the
    // user provides a system id where we're not expecting it, _then_
    // we'll specify a non-system identifier.
    def printable = "an identifier"
  }
  case object AQualifiedIdentifier extends Expectation {
    // Ditto re: the note on AnIdentifier
    def printable = "a qualified identifier"
  }
  case object ASystemIdentifier extends Expectation {
    def printable = "a system identifier"
  }
  case object AQualifiedSystemIdentifier extends Expectation {
    def printable = "a qualified system identifier"
  }
  case object ANonSystemIdentifier extends Expectation {
    def printable = "a non-system identifier"
  }
  case object ATableIdentifier extends Expectation {
    def printable = "a table identifier"
  }
  case object AnExpression extends Expectation {
    def printable = "an expression"
  }
  case object AGroupBy extends Expectation {
    def printable = "`GROUP BY'"
  }
  case object AnOrderBy extends Expectation {
    def printable = "`ORDER BY'"
  }
  case object APartitionBy extends Expectation {
    def printable = "`PARTITION BY'"
  }
  case object AnIsNot extends Expectation {
    def printable = "`IS NOT'"
  }
  case object ANotLike extends Expectation {
    def printable = "`NOT LIKE'"
  }
  case object ANotBetween extends Expectation {
    def printable = "`NOT BETWEEN'"
  }
  case object ANotIn extends Expectation {
    def printable = "`NOT IN'"
  }
  case object AnAliasForThis extends Expectation {
    def printable = "an alias for `@this'"
  }
  case object AnOperator extends Expectation {
    def printable = "an operator"
  }
  case object AHint extends Expectation {
    def printable = "a hint"
  }

  implicit def tokenAsTokenLike(t: Token): Expectation = ActualToken(t)

  def expectationsToEnglish(expectation: Iterable[Expectation], got: Token): String = {
    val sb = new StringBuilder
    sb.append("Expected")

    def loop(expectations: LazyList[RecursiveDescentParser.Expectation], n: Int): Unit = {
      expectations match {
        case LazyList() =>
          // Uhhh... this shouldn't happen
          sb.append(" nothing")
        case LazyList.cons(hd, LazyList()) =>
          if(n == 1) sb.append(" or ")
          else if(n > 1) sb.append(", or ") // oxford comma 4eva
          else sb.append(' ')
          sb.append(hd.printable)
        case LazyList.cons(hd, tl) =>
          if(n == 0) sb.append(" one of ")
          else sb.append(", ")
          sb.append(hd.printable)
          loop(tl, n+1)
      }
    }
    loop(expectation.to(LazyList), 0)

    sb.append(", but got ").
      append(got.quotedPrintable).
      toString
  }

  val CASE = new Keyword("CASE")
  val WHEN = new Keyword("WHEN")
  val THEN = new Keyword("THEN")
  val ELSE = new Keyword("ELSE")
  val END = new Keyword("END")
  val OVER = new Keyword("OVER")
  val PARTITION = new Keyword("PARTITION")
  val BY = new Keyword("BY")
  val NULLS = new Keyword("NULLS")
  val FIRST = new Keyword("FIRST")
  val LAST = new Keyword("LAST")
  val RANGE = new Keyword("RANGE")
  val ROWS = new Keyword("ROWS")
  val UNBOUNDED = new Keyword("UNBOUNDED")
  val PRECEDING = new Keyword("PRECEDING")
  val CURRENT = new Keyword("CURRENT")
  val ROW = new Keyword("ROW")
  val FOLLOWING = new Keyword("FOLLOWING")
  val FILTER = new Keyword("FILTER")

  // Hints
  val HINT = new Keyword("HINT")
  val MATERIALIZED = new Keyword("MATERIALIZED")
  val NO_ROLLUP = new Keyword("NO_ROLLUP")
  val NO_CHAIN_MERGE = new Keyword("NO_CHAIN_MERGE")

  sealed abstract class NullPlacement
  case object First extends NullPlacement
  case object Last extends NullPlacement

  type Reader = ExtendedReader[Token]

  trait ParseException extends Exception {
    def position: Position = reader.first.position
    val reader: Reader
  }

  case class ParseResult[+T](reader: Reader, value: T) {
    def map[U](f: T=>U): ParseResult[U] = copy(value = f(value))
  }

  // sigh... ut's unfortunate that these are all here rather than
  // being close to the expectations they reflect, but alas Scala
  // provides no way to create a nontrivial immutable value and have
  // it cached other than by sticking it in a `val` on some object,
  // and recreating these sets on-site has a fairly hefty runtime
  // cost.
  private[this] def s(xs: Expectation*) = ListSet(xs : _*)
  private val AND_SET = s(AND())
  private val LIMIT_OFFSET_SET = s(LIMIT(), OFFSET())
  private val LIMIT_SET = s(LIMIT())
  private val OFFSET_SET = s(OFFSET())
  private val ORDERBY_SEARCH_SET = s(AnOrderBy, SEARCH())
  private val ORDERBY_SET = s(AnOrderBy)
  private val SEARCH_SET = s(SEARCH())
  private val GROUPBY_SET = s(AGroupBy)
  private val WHERE_SET = s(WHERE())
  private val HAVING_SET = s(HAVING())
  private val TABLEID_COLONSTAR_SET = s(ATableIdentifier, COLONSTAR())
  private val LPAREN_SET = s(LPAREN())
  private val COMMA_SET = s(COMMA())
  private val TABLEID_STAR_SET = s(ATableIdentifier, STAR())
  private val AS_SET = s(AS())
  private val EXPRESSION_SET = s(AnExpression)
  private val JOIN_SET = s(LEFT(), RIGHT(), FULL(), JOIN())
  private val AS_LPAREN_SET = s(AS(), LPAREN())
  private val TABLEID_SET = s(ATableIdentifier)
  private val QUERYPIPE_SET = s(QUERYPIPE())
  private val ROWOP_SET = s(QUERYPIPE(), QUERYUNION(), QUERYINTERSECT(), QUERYMINUS(), QUERYUNIONALL(), QUERYINTERSECTALL(), QUERYMINUSALL())
  private val DISTINCT_SET = s(DISTINCT())
  private val FROM_SET = s(FROM())
  private val PARTITIONBY_SET = s(APartitionBy)
  private val ASC_DESC_SET = s(ASC(), DESC())
  private val NULL_NULLS_SET = s(NULL(), NULLS())
  private val RANGE_ROWS_SET = s(RANGE(), ROWS())
  private val OVER_SET = s(OVER())
  private val FILTER_SET = s(FILTER())
  private val DOT_LBRACKET_SET = s(DOT(), LBRACKET())

  private val COLONCOLON_SET = s(COLONCOLON())
  private val MINUS_PLUS_SET = s(MINUS(), PLUS())

  // These are all collapsed into "an operator" for ease of
  // interpretation by an end-user instead of getting a bunch of
  // line-noise.  Leaving them here in case we want to bring them back
  // for some reason...

  // private val CARET_SET = s(CARET())
  // private val FACTOR_SET = s(STAR(), SLASH(), PERCENT())
  // private val TERM_SET = s(PLUS(), MINUS(), PIPEPIPE())
  // private val COMPARISON_SET = s(EQUALS(), LESSGREATER(), LESSTHAN(), LESSTHANOREQUALS(), GREATERTHAN(), GREATERTHANOREQUALS(), EQUALSEQUALS(), BANGEQUALS())
  private val OPERATOR_SET = s(AnOperator)

  private val LIKE_BETWEEN_IN_SET = s(IS(), AnIsNot, LIKE(), ANotLike, BETWEEN(), ANotBetween, IN(), ANotIn)
  private val NOT_SET = s(NOT())
  private val OR_SET = s(OR())
}

abstract class RecursiveDescentParser(parameters: AbstractParser.Parameters = AbstractParser.defaultParameters) extends AbstractParser with RecursiveDescentHintParser {
  import parameters._
  import RecursiveDescentParser._

  // These are the things that need implementing
  protected def lexer(s: String): AbstractLexer
  protected def expected(reader: Reader): ParseException
  protected def expectedLeafQuery(reader: Reader): ParseException
  protected def unexpectedStarSelect(reader: Reader): ParseException
  protected def unexpectedSystemStarSelect(reader: Reader): ParseException

  def binaryTreeSelect(soql: String): BinaryTree[Select] = parseFull(compoundSelect, soql)

  def selection(soql: String): Selection = parseFull(selectList, soql)
  def joins(soql: String): Seq[Join] = parseFull(joinList, soql)
  def expression(soql: String): Expression = parseFull(topLevelExpr, soql)
  def orderings(soql: String): Seq[OrderBy] = parseFull(orderingList, soql)
  def groupBys(soql: String): Seq[Expression] = parseFull(commaSeparatedExprs, soql)
  def distinct(soql: String): Distinctiveness = parseFull(distinctiveness, soql)

  def selectStatement(soql: String): NonEmptySeq[Select] = parseFull(pipedSelect, soql)
  def unchainedSelectStatement(soql: String): Select = parseFull(select, soql)
  def parseJoinSelect(soql: String): JoinSelect = parseFull(joinSelect, soql)
  def limit(soql: String): BigInt = parseFull(integerLiteral, soql)
  def offset(soql: String): BigInt = parseFull(integerLiteral, soql)
  def search(soql: String): String = parseFull(stringLiteral, soql)
  def hints(soql: String): Seq[Hint] = parseFull(hints, soql)

  protected final def fail(reader: Reader, expectation: Expectation, expectations: Expectation*): Nothing = {
    val ls = ListSet.newBuilder[Expectation]
    ls += expectation
    ls ++= expectations
    reader.addAlternates(ls.result())
    throw expected(reader)
  }

  protected final def parseFull[T](parser: Reader => ParseResult[T], soql: String): T = {
    val ParseResult(end, result) = parser(new LexerReader(lexer(soql)))
    if(end.first.isInstanceOf[EOF]) {
      result
    } else {
      fail(end, EOF())
    }
  }

  protected final def integerLiteral(reader: Reader): ParseResult[BigInt] = {
    reader.first match {
      case n: IntegerLiteral =>
        ParseResult(reader.rest, n.asInt)
      case _ =>
        fail(reader, AnIntegerLiteral)
    }
  }

  protected final def stringLiteral(reader: Reader): ParseResult[String] = {
    reader.first match {
      case s: tokens.StringLiteral =>
        ParseResult(reader.rest, s.value)
      case _ =>
        fail(reader, AStringLiteral)
    }
  }

  protected final def select(reader: Reader): ParseResult[Select] = {
    reader.first match {
      case SELECT() =>
        val ParseResult(r1, h) = hints(reader.rest)
        val ParseResult(r2, distinct) = distinctiveness(r1)
        val ParseResult(r3, selected) = selectList(r2)
        val ParseResult(r4, fromClause) = from(r3)
        val ParseResult(r5, joinClause) = if(allowJoins) joinList(r4) else ParseResult(r4, Seq.empty)
        val ParseResult(r6, whereClause) = where(r5)
        val ParseResult(r7, groupByClause) = groupBy(r6)
        val ParseResult(r8, havingClause) = having(r7)
        val ParseResult(r9, (orderByClause, searchClause)) = orderByAndSearch(r8)
        val ParseResult(r10, (limitClause, offsetClause)) = limitOffset(r9)
        ParseResult(r10, Select(distinct, selected, fromClause, joinClause, whereClause, groupByClause, havingClause, orderByClause, limitClause, offsetClause, searchClause, h).validate())
      case _ =>
        fail(reader, SELECT())
    }
  }

  protected final def limitOffset(reader: Reader): ParseResult[(Option[BigInt], Option[BigInt])] = {
    reader.first match {
      case LIMIT() =>
        val ParseResult(r2, limitClause) = limit(reader)
        val ParseResult(r3, offsetClause) = offset(r2)
        ParseResult(r3, (limitClause, offsetClause))
      case OFFSET() =>
        val ParseResult(r2, offsetClause) = offset(reader)
        val ParseResult(r3, limitClause) = limit(r2)
        ParseResult(r3, (limitClause, offsetClause))
      case _ =>
        reader.addAlternates(LIMIT_OFFSET_SET)
        ParseResult(reader, (None, None))
    }
  }

  protected final def limit(reader: Reader): ParseResult[Option[BigInt]] = {
    reader.first match {
      case LIMIT() =>
        reader.rest.first match {
          case lit: IntegerLiteral => ParseResult(reader.rest.rest, Some(lit.asInt))
          case _ => fail(reader.rest, AnIntegerLiteral)
        }
      case _ =>
        reader.addAlternates(LIMIT_SET)
        ParseResult(reader, None)
    }
  }


  protected final def offset(reader: Reader): ParseResult[Option[BigInt]] = {
    reader.first match {
      case OFFSET() =>
        reader.rest.first match {
          case lit: IntegerLiteral => ParseResult(reader.rest.rest, Some(lit.asInt))
          case _ => fail(reader.rest, AnIntegerLiteral)
        }
      case _ =>
        reader.addAlternates(OFFSET_SET)
        ParseResult(reader, None)
    }
  }

  protected final def orderByAndSearch(reader: Reader): ParseResult[(Seq[OrderBy], Option[String])] = {
    reader.first match {
      case ORDER() =>
        val ParseResult(r2, orderByClause) = orderBy(reader)
        val ParseResult(r3, searchClause) = search(r2)
        ParseResult(r3, (orderByClause, searchClause))
      case SEARCH() =>
        val ParseResult(r2, searchClause) = search(reader)
        val ParseResult(r3, orderByClause) = orderBy(r2)
        ParseResult(r3, (orderByClause, searchClause))
      case _ =>
        reader.addAlternates(ORDERBY_SEARCH_SET)
        ParseResult(reader, (Nil, None))
    }
  }

  protected final def orderBy(reader: Reader): ParseResult[Seq[OrderBy]] = {
    reader.first match {
      case ORDER() =>
        reader.rest.first match {
          case BY() =>
            orderingList(reader.rest.rest)
          case _ =>
            fail(reader.rest, BY())
        }
      case _ =>
        reader.addAlternates(ORDERBY_SET)
        ParseResult(reader, Nil)
    }
  }

  protected final def search(reader: Reader): ParseResult[Option[String]] = {
    reader.first match {
      case SEARCH() =>
        reader.rest.first match {
          case tokens.StringLiteral(s) =>
            ParseResult(reader.rest.rest, Some(s))
          case _ =>
            fail(reader.rest, AStringLiteral)
        }
      case _ =>
        reader.addAlternates(SEARCH_SET)
        ParseResult(reader, None)
    }
  }

  protected final def groupBy(reader: Reader): ParseResult[Seq[Expression]] = {
    reader.first match {
      case GROUP() =>
        reader.rest.first match {
          case BY() =>
            val r = commaSeparatedExprs(reader.rest.rest)
            r.reader.resetAlternates()
            r
          case _ =>
            fail(reader, BY())
        }
      case _ =>
        reader.addAlternates(GROUPBY_SET)
        ParseResult(reader, Nil)
    }
  }

  protected final def where(reader: Reader): ParseResult[Option[Expression]] = {
    reader.first match {
      case WHERE() =>
        topLevelExpr(reader.rest).map(Some(_))
      case _ =>
        reader.addAlternates(WHERE_SET)
        ParseResult(reader, None)
    }
  }

  private final def having(reader: Reader): ParseResult[Option[Expression]] = {
    reader.first match {
      case HAVING() =>
        topLevelExpr(reader.rest).map(Some(_))
      case _ =>
        reader.addAlternates(HAVING_SET)
        ParseResult(reader, None)
    }
  }

  private final def selectList(reader: Reader): ParseResult[Selection] = {
    val ParseResult(r2, systemStar) = systemStarSelection(reader)
    val ParseResult(r3, userStars) = userStarSelections(r2, systemStar.isDefined)
    val ParseResult(r4, expressions) = expressionSelectList(r3, systemStar.isDefined || userStars.nonEmpty)
    ParseResult(r4, Selection(systemStar, userStars, expressions))
  }

  private def systemStarSelection(reader: Reader): ParseResult[Option[StarSelection]] = {
    reader.first match {
      case ti: TableIdentifier =>
        reader.rest.first match {
          case DOT() =>
            reader.rest.rest.first match {
              case star@COLONSTAR() =>
                selectExceptions(reader.rest.rest.rest, unqualifiedSystemIdentifier).map { exceptions =>
                  Some(StarSelection(Some(intoQualifier(ti)), exceptions).positionedAt(star.position))
                }
              case _ =>
                // whoops, we're not parsing a :* at all, bail out
                reader.addAlternates(TABLEID_COLONSTAR_SET)
                ParseResult(reader, None)
            }
          case _ =>
            fail(reader.rest, DOT())
        }
      case star@COLONSTAR() =>
        selectExceptions(reader.rest, unqualifiedSystemIdentifier).map { exceptions =>
          Some(StarSelection(None, exceptions).positionedAt(star.position))
        }
      case _ =>
        reader.addAlternates(TABLEID_COLONSTAR_SET)
        ParseResult(reader, None)
    }
  }

  private def selectExceptions(reader: Reader, identParser: Reader => ParseResult[(ColumnName, Position)]): ParseResult[Seq[(ColumnName, Position)]] = {
    reader.first match {
      case LPAREN() =>
        reader.rest.first match {
          case EXCEPT() =>
            val result = Vector.newBuilder[(ColumnName, Position)]
            @tailrec
            def loop(reader: Reader): Reader = {
              val ParseResult(r2, ident) = identParser(reader)
              result += ident
              r2.first match {
                case COMMA() =>
                  loop(r2.rest)
                case RPAREN() =>
                  r2.rest
                case _ =>
                  fail(r2, COMMA(), RPAREN())
              }
            }
            val finalReader = loop(reader.rest.rest)
            ParseResult(finalReader, result.result())
          case _ =>
            fail(reader.rest, EXCEPT())
        }
      case _ =>
        reader.addAlternates(LPAREN_SET)
        ParseResult(reader, Nil)
    }
  }

  protected final def distinctiveness(reader: Reader): ParseResult[Distinctiveness] = {
    reader.first match {
      case DISTINCT() =>
        reader.rest.first match {
          case ON() =>
            reader.rest.rest.first match {
              case LPAREN() =>
                val pr = commaSeparatedExprs(reader.rest.rest.rest)
                pr.reader.first match {
                  case RPAREN() =>
                    DistinctOn(pr.value)
                    ParseResult(pr.reader.rest, DistinctOn(pr.value))
                  case _ =>
                    fail(reader, RPAREN())
                }
              case _ =>
                fail(reader, LPAREN())
            }
          case _ =>
            ParseResult(reader.rest, FullyDistinct)
        }
      case _ =>
        reader.addAlternates(DISTINCT_SET)
        ParseResult(reader, Indistinct)
    }
  }

  protected final def unqualifiedSystemIdentifier(reader: Reader): ParseResult[(ColumnName, Position)] = {
    reader.first match {
      case si: SystemIdentifier =>
        ParseResult(reader.rest, (ColumnName(si.value), si.position))
      case _ =>
        fail(reader, ASystemIdentifier)
    }
  }

  protected final def simpleIdentifier(reader: Reader): ParseResult[(String, Position)] =
    reader.first match {
      case i: Identifier => ParseResult(reader.rest, (i.value, i.position))
      case _ : SystemIdentifier => fail(reader, ANonSystemIdentifier)
      case _ => fail(reader, AnIdentifier)
    }

  protected final def unqualifiedUserIdentifier(reader: Reader): ParseResult[(ColumnName, Position)] = {
    simpleIdentifier(reader).map { case (n, p) => (ColumnName(n), p) }
  }

  protected final def unqualifiedIdentifier(reader: Reader): ParseResult[(ColumnName, Position)] = {
    reader.first match {
      case i: Identifier =>
        ParseResult(reader.rest, (ColumnName(i.value), i.position))
      case si: SystemIdentifier =>
        ParseResult(reader.rest, (ColumnName(si.value), si.position))
      case _ =>
        fail(reader, AnIdentifier)
    }
  }

  private def userStarSelections(reader: Reader, commaFirst: Boolean): ParseResult[Seq[StarSelection]] = {
    // This is a little tricky, because we may or may not be parsing
    // anything at all, so we need to keep track of when we've
    // committed to a thing.  Basically we're parsing either
    //     (COMMA userStarExpr)*
    // if commaFirst is true and
    //     (userStarExpr (COMMA userStarExpr)*)?
    // if it's false, and we're only committed to completing a phrase
    // at the points where we've actually seen a STAR token

    val result = Vector.newBuilder[StarSelection]

    @tailrec
    def loop(reader: Reader, commaFirst: Boolean): Reader = {
      val r =
        if(commaFirst) {
          reader.first match {
            case COMMA() =>
              reader.rest
            case _ =>
              // We require a comma but do not see one - revert.
              reader.addAlternates(COMMA_SET)
              return reader
          }
        } else {
          reader
        }

      r.first match {
        case ti: TableIdentifier =>
          r.rest.first match {
            case DOT() =>
              r.rest.rest.first match {
                case star@STAR() =>
                  // We are now committed to parsing a user-star-selection
                  val ParseResult(r2, exceptions) = selectExceptions(r.rest.rest.rest, unqualifiedUserIdentifier)
                  result += StarSelection(Some(intoQualifier(ti)), exceptions).positionedAt(star.position)
                  loop(r2, commaFirst = true)
                case star@COLONSTAR() =>
                  throw unexpectedSystemStarSelect(r.rest.rest)
                case _ =>
                  // Whoops, not a star selection, revert
                  reader.addAlternates(TABLEID_STAR_SET)
                  reader
              }
            case _ =>
              reader.addAlternates(TABLEID_STAR_SET)
              reader
          }
        case star@STAR() =>
          val ParseResult(r2, exceptions) = selectExceptions(r.rest, unqualifiedUserIdentifier)
          result += StarSelection(None, exceptions).positionedAt(star.position)
          loop(r2, commaFirst = true)
        case star@COLONSTAR() =>
          throw unexpectedSystemStarSelect(r)
        case _ =>
          reader.addAlternates(TABLEID_STAR_SET)
          reader
      }
    }

    val finalReader = loop(reader, commaFirst)
    ParseResult(finalReader, result.result())
  }

  private def expressionSelectList(reader: Reader, commaFirst: Boolean): ParseResult[Seq[SelectedExpression]] = {
    // similar to the user select list, we need to track our commits
    val result = Vector.newBuilder[SelectedExpression]

    @tailrec
    def loop(reader: Reader, commaFirst: Boolean): Reader = {
      // Unlike the user select list above, if we need to see a comma
      // and we see one, we're committed to parsing an expression.
      // Otherwise we might just be at the end of the list!

      val r =
        if(commaFirst) {
          reader.first match {
            case COMMA() =>
              reader.rest
            case _ =>
              // We require a comma but do not see one - revert.
              reader.addAlternates(COMMA_SET)
              return reader
          }
        } else {
          reader
        }

      // This is written in a slightly strange way to placate the
      // tail-recursion checker (it's still lexically tail-recursive
      // if the `catch` does a `return reader` but apparently that
      // does something internally which causes it not to be)

      // We'll do a little lookahead to see if this is a star-select
      // or a qualified-star-select, and if it is we'll complain
      r.toStream.take(3) match {
        case Seq(_ : TableIdentifier, DOT(), STAR() | COLONSTAR()) | Seq(STAR() | COLONSTAR(), _ @ _*) =>
          throw unexpectedStarSelect(r)
        case _ =>
          // ok, it's not
      }

      val exprResult =
        try {
          Some(topLevelExpr(r))
        } catch {
          case e: ParseException if !commaFirst && e.reader == r =>
            // ok, so if the parse failed right at the start we'll
            // assume we're good at the end of the select list and are
            // good to move on...
            None
        }

      exprResult match {
        case Some(ParseResult(r2, e)) =>
          r2.first match {
            case AS() =>
              // now we must see an identifier...
              val ParseResult(r3, identPos) = alias(r2.rest)
              result += SelectedExpression(e, Some(identPos))
              loop(r3, commaFirst = true)
            case _ =>
              r2.addAlternates(AS_SET)
              result += SelectedExpression(e, None)
              loop(r2, commaFirst = true)
          }
        case None =>
          reader.addAlternates(EXPRESSION_SET)
          reader
      }
    }

    val finalReader = loop(reader, commaFirst)
    ParseResult(finalReader, result.result())
  }

  private def alias(reader: Reader): ParseResult[(ColumnName, Position)] = {
    reader.first match {
      case i: Identifier =>
        ParseResult(reader.rest, (ColumnName(i.value), i.position))
      case si: SystemIdentifier =>
        val cn = ColumnName(si.value)
        if(parameters.systemColumnAliasesAllowed.contains(cn)) {
          ParseResult(reader.rest, (cn, si.position))
        } else {
          fail(reader, ANonSystemIdentifier)
        }
      case _ =>
        fail(reader, AnIdentifier)
    }
  }

  protected final def joinList(reader: Reader): ParseResult[Seq[Join]] = {
    val acc = Vector.newBuilder[Join]

    @tailrec
    def loop(reader: Reader): Reader = {
      val ParseResult(r2, joinClause) = join(reader)
      joinClause match {
        case Some(j) =>
          acc += j
          loop(r2)
        case None =>
          r2
      }
    }

    val finalReader = loop(reader)
    ParseResult(finalReader, acc.result())
  }

  protected final def join(reader: Reader): ParseResult[Option[Join]] = {
    reader.first match {
      case direction@(LEFT() | RIGHT() | FULL()) =>
        val r2 = reader.rest.first match {
          case OUTER() => reader.rest.rest
          case _ => reader.rest
        }
        r2.first match {
          case JOIN() =>
            finishJoin(r2.rest, OuterJoin(direction, _, _, _)).map(Some(_))
          case _ =>
            fail(reader, JOIN())
        }
      case JOIN() =>
        finishJoin(reader.rest, InnerJoin(_, _, _)).map(Some(_))
      case _ =>
        reader.addAlternates(JOIN_SET)
        ParseResult(reader, None)
    }
  }

  private def finishJoin(reader: Reader, join: (JoinSelect, Expression, Boolean) => Join): ParseResult[Join] = {
    val (r2, isLateral) = reader.first match {
      case LATERAL() => (reader.rest, true)
      case _ => (reader, false)
    }
    val ParseResult(r3, select) = joinSelect(r2)
    r3.first match {
      case ON() =>
        topLevelExpr(r3.rest).map(join(select, _, isLateral))
      case _ =>
        fail(r3, ON())
    }
  }

  protected final def joinSelect(reader: Reader): ParseResult[JoinSelect] = {
    reader.first match {
      case ti: TableIdentifier =>
        // could be JoinTable, could be JoinFunc if that's allowed here
        reader.rest.first match {
          case AS() =>
            // JoinTable
            tableAlias(reader.rest.rest).map { a =>
              JoinTable(TableName(intoQualifier(ti), Some(a)))
            }
          case LPAREN() if allowJoinFunctions =>
            // JoinFunc
            val ParseResult(r2, args) = parseArgList(reader.rest.rest)
            r2.first match {
              case AS() =>
                tableAlias(r2.rest).map { a =>
                  JoinFunc(TableName(intoQualifier(ti), Some(a)), args)(ti.position)
                }
              case _ =>
                r2.addAlternates(AS_SET)
                ParseResult(r2, JoinFunc(TableName(intoQualifier(ti), None), args)(ti.position))
            }
          case _ =>
            // Also JoinTable
            reader.rest.addAlternates(AS_LPAREN_SET)
            ParseResult(reader.rest, JoinTable(TableName(intoQualifier(ti), None)))
        }
      case _ =>
        reader.addAlternates(TABLEID_SET)
        val ParseResult(r2, s) = atomSelect(reader)
        r2.first match {
          case AS() =>
            tableAlias(r2.rest).map(JoinQuery(s, _))
          case _ =>
            fail(r2, AS())
        }
    }
  }

  protected final def atomSelect(reader: Reader): ParseResult[BinaryTree[Select]] = {
    reader.first match {
      case LPAREN() =>
        parenSelect(reader.rest)
      case _ =>
        reader.addAlternates(LPAREN_SET)
        select(reader).map(Leaf(_))
    }
  }

  // already past the lparen
  private def parenSelect(reader: Reader): ParseResult[BinaryTree[Select]] = {
    val ParseResult(r2, s) = compoundSelect(reader)
    r2.first match {
      case RPAREN() =>
        ParseResult(r2.rest, s)
      case _ =>
        fail(r2, RPAREN())
    }
  }

  protected final def pipedSelect(reader: Reader): ParseResult[NonEmptySeq[Select]] = {
    val ParseResult(r2, s1) = select(reader)
    val tail = Vector.newBuilder[Select]

    @tailrec
    def loop(reader: Reader): Reader = {
      reader.first match {
        case QUERYPIPE() =>
          val ParseResult(r2, s) = select(reader.rest)
          tail += s
          loop(r2)
        case _ =>
          reader.addAlternates(QUERYPIPE_SET)
          reader
      }
    }
    val finalReader = loop(r2)

    ParseResult(finalReader, NonEmptySeq(s1, tail.result()))
  }

  protected final def compoundSelect(reader: Reader): ParseResult[BinaryTree[Select]] = {
    val ParseResult(r2, s) = atomSelect(reader)
    compoundSelect_!(r2, s)
  }

  @tailrec
  private def compoundSelect_!(reader: Reader, arg: BinaryTree[Select]): ParseResult[BinaryTree[Select]] = {
    reader.first match {
      case QUERYPIPE() =>
        val ParseResult(r2, arg2) = atomSelect(reader.rest)
        arg2.asLeaf match {
          case Some(leaf) =>
            compoundSelect_!(r2, PipeQuery(arg, Leaf(leaf)))
          case None =>
            // We need arg2 to be a leaf query, and we got a compound
            // query, so reset any alternate expectations we have and
            // fail with that specific error.
            reader.rest.resetAlternates()
            throw expectedLeafQuery(reader.rest)
        }
      case op@(QUERYUNION() | QUERYINTERSECT() | QUERYMINUS() | QUERYUNIONALL() | QUERYINTERSECTALL() | QUERYMINUSALL()) =>
        val ParseResult(r2, arg2) = atomSelect(reader.rest)
        compoundSelect_!(r2, Compound(op.printable, arg, arg2))
      case _ =>
        reader.addAlternates(ROWOP_SET)
        ParseResult(reader, arg)
    }
  }

  protected final def distinct(reader: Reader): ParseResult[Boolean] = {
    reader.first match {
      case DISTINCT() =>
        ParseResult(reader.rest, true)
      case _ =>
        reader.addAlternates(DISTINCT_SET)
        ParseResult(reader, false)
    }
  }

  protected final def from(reader: Reader): ParseResult[Option[TableName]] = {
    reader.first match {
      case FROM() =>
        // sadness: this loses position information
        val tn =
          reader.rest.first match {
            case tn: TableIdentifier => intoQualifier(tn)
            case _ => fail(reader.rest, ATableIdentifier)
          }
        val ParseResult(r2, alias) =
          reader.rest.rest.first match {
            case AS() =>
              tableAlias(reader.rest.rest.rest).map(Some(_))
            case _ =>
              ParseResult(reader.rest.rest, None)
          }
        val tableName = TableName(tn, alias)
        if(tableName.nameWithSodaFountainPrefix == TableName.This && tableName.alias.isEmpty) {
          // We need an alias, so report an error at the point where
          // we did NOT see the AS token.
          reader.rest.rest.resetAlternates()
          fail(reader.rest.rest, AnAliasForThis)
        }
        ParseResult(r2, Some(tableName))
      case _ =>
        reader.addAlternates(FROM_SET)
        ParseResult(reader, None)
    }
  }

  private def tableAlias(reader: Reader): ParseResult[String] = {
    val ta =
      reader.first match {
        case ident: Identifier => ident.value
        case ident: TableIdentifier => intoQualifier(ident)
        case _ => fail(reader, ATableIdentifier) // This is a lie because we also accept an unadorned identifier, but that's legacy syntax
      }
    ParseResult(reader.rest, TableName.withSodaFountainPrefix(ta))
  }

  private def conditional(reader: Reader, casePos: Position): ParseResult[Expression] = {
    // we're given a reader that's already past the CASE, so we just
    // need to parse (WHEN expr THEN expr)+ ELSE expr END
    val cases = Vector.newBuilder[Expression]

    @tailrec
    def loop(reader: Reader): Either[Reader, Reader] = {
      // this is in a reader that has consumed the WHEN and therefore is reading a condition
      val ParseResult(r2, condition) = nestedExpr(reader)
      r2.first match {
        case THEN() =>
          val ParseResult(r3, consequent) = nestedExpr(r2.rest)
          cases += condition
          cases += consequent
          r3.first match {
            case WHEN() =>
              loop(r3.rest)
            case ELSE() =>
              Left(r3.rest)
            case END() =>
              Right(r3.rest)
            case _ =>
              fail(r3, WHEN(), ELSE(), END())
          }
        case _ =>
          fail(r2, THEN())
      }
    }

    reader.first match {
      case WHEN() =>
        val finalReader =
          loop(reader.rest) match {
            case Left(elseReader) =>
              val ParseResult(r2, alternate) = nestedExpr(elseReader)
              cases += ast.BooleanLiteral(true)(casePos)
              cases += alternate
              r2.first match {
                case END() => r2.rest
                case _ => fail(r2, END())
              }
            case Right(endReader) =>
              cases += ast.BooleanLiteral(false)(casePos)
              cases += ast.NullLiteral()(casePos)
              endReader
          }
        ParseResult(finalReader, FunctionCall(SpecialFunctions.Case, cases.result(), None)(casePos, casePos))
      case _ =>
        fail(reader, WHEN())
    }
  }

  protected final def commaSeparatedExprs(reader: Reader): ParseResult[Seq[Expression]] = {
    val args = Vector.newBuilder[Expression]

    @tailrec
    def loop(reader: Reader): Reader = {
      val ParseResult(r2, candidate) = nestedExpr(reader)
      args += candidate
      r2.first match {
        case COMMA() =>
          loop(r2.rest)
        case _ =>
          r2.addAlternates(COMMA_SET)
          r2
      }
    }
    val finalReader = loop(reader)

    ParseResult(finalReader, args.result())
  }

  private def windowPartitionBy(reader: Reader): ParseResult[Seq[Expression]] = {
    reader.first match {
      case PARTITION() =>
        reader.rest.first match {
          case BY() => commaSeparatedExprs(reader.rest.rest)
          case _ => fail(reader.rest, BY())
        }
      case _ =>
        reader.addAlternates(PARTITIONBY_SET)
        ParseResult(reader, Nil)
    }
  }

  private def maybeAscDesc(reader: Reader): ParseResult[OrderDirection] = {
    reader.first match {
      case a@ASC() => ParseResult(reader.rest, a)
      case d@DESC() => ParseResult(reader.rest, d)
      case _ =>
        reader.addAlternates(ASC_DESC_SET)
        ParseResult(reader, ASC())
    }
  }

  private def maybeNullPlacement(reader: Reader): ParseResult[Option[NullPlacement]] = {
    reader.first match {
      case NULL() | NULLS() =>
        val direction =
          reader.rest.first match {
            case FIRST() => Some(First)
            case LAST() => Some(Last)
            case _ => fail(reader.rest, FIRST(), LAST())
          }
        ParseResult(reader.rest.rest, direction)
      case _ =>
        reader.addAlternates(NULL_NULLS_SET)
        ParseResult(reader, None)
    }
  }

  protected final def ordering(reader: Reader): ParseResult[OrderBy] = {
    val ParseResult(r2, criterion) = topLevelExpr(reader)
    val ParseResult(r3, ascDesc) = maybeAscDesc(r2)
    val ParseResult(r4, firstLast) = maybeNullPlacement(r3)

    ParseResult(r4, OrderBy(criterion, ascDesc == ASC(), firstLast.fold(ascDesc == ASC())(_ == Last)))
  }

  protected final def orderingList(reader: Reader): ParseResult[Seq[OrderBy]] = {
    val args = Vector.newBuilder[OrderBy]

    @tailrec
    def loop(reader: Reader): Reader = {
      val ParseResult(r2, candidate) = ordering(reader)
      args += candidate
      r2.first match {
        case COMMA() =>
          loop(r2.rest)
        case _ =>
          r2.addAlternates(Set[Expectation](COMMA()))
          r2
      }
    }
    val finalReader = loop(reader)

    ParseResult(finalReader, args.result())
  }

  private def windowOrderBy(reader: Reader): ParseResult[Seq[OrderBy]] = {
    reader.first match {
      case ORDER() =>
        reader.rest.first match {
          case BY() => orderingList(reader.rest.rest)
          case _ => fail(reader.rest, BY())
        }
      case _ =>
        reader.addAlternates(ORDERBY_SET)
        ParseResult(reader, Nil)
    }
  }

  private def tokenToLiteral(token: Token): Literal = {
    ast.StringLiteral(token.printable.toUpperCase)(token.position)
  }

  private def frameStartEnd(reader: Reader): ParseResult[Seq[Expression]] = {
    reader.first match {
      case a@UNBOUNDED() =>
        reader.rest.first match {
          case b@PRECEDING() =>
            ParseResult(reader.rest.rest, Seq(tokenToLiteral(a), tokenToLiteral(b)))
          case _ =>
            fail(reader.rest, PRECEDING())
        }
      case a@CURRENT() =>
        reader.rest.first match {
          case b@ROW() =>
            ParseResult(reader.rest.rest, Seq(tokenToLiteral(a), tokenToLiteral(b)))
          case _ =>
            fail(reader.rest, ROW())
        }
      case n: IntegerLiteral =>
        reader.rest.first match {
          case b@(PRECEDING() | FOLLOWING()) =>
            ParseResult(reader.rest.rest, Seq(ast.NumberLiteral(BigDecimal(n.asInt))(n.position), tokenToLiteral(b)))
          case _ =>
            fail(reader.rest, PRECEDING(), FOLLOWING())
        }
      case _ =>
        fail(reader, UNBOUNDED(), CURRENT(), AnIntegerLiteral)
    }
  }

  private def windowFrameClause(reader: Reader): ParseResult[Seq[Expression]] = {
    reader.first match {
      case r@(RANGE() | ROWS()) =>
        reader.rest.first match {
          case b@BETWEEN() =>
            val ParseResult(r2, fa) = frameStartEnd(reader.rest.rest)
            r2.first match {
              case a@AND() =>
                frameStartEnd(r2.rest).map { fb =>
                  Seq(tokenToLiteral(r), tokenToLiteral(b)) ++ fa ++ Seq(tokenToLiteral(a)) ++ fb
                }
              case _ =>
                fail(r2, AND())
            }
          case UNBOUNDED() | CURRENT() =>
            frameStartEnd(reader.rest).map { fb =>
              tokenToLiteral(r) +: fb
            }
          case _: IntegerLiteral =>
            frameStartEnd(reader.rest).map { fb =>
              tokenToLiteral(r) +: fb
            }
          case _ =>
            fail(reader.rest, BETWEEN(), UNBOUNDED(), CURRENT(), AnIntegerLiteral)
        }
      case _ =>
        reader.addAlternates(RANGE_ROWS_SET)
        ParseResult(reader, Nil)
    }
  }

  private def windowFunctionParams(reader: Reader): ParseResult[WindowFunctionInfo] = {
    reader.first match {
      case LPAREN() =>
        val ParseResult(r2, partition) = windowPartitionBy(reader.rest)
        val ParseResult(r3, orderBy) = windowOrderBy(r2)
        val ParseResult(r4, frame) = windowFrameClause(r3)
        r4.first match {
          case RPAREN() =>
            ParseResult(r4.rest, WindowFunctionInfo(partition, orderBy, frame))
          case _ =>
            fail(r4, RPAREN())
        }
      case _ =>
        fail(reader, LPAREN())
    }
  }

  private def parseFunctionOver(reader: Reader): ParseResult[Option[WindowFunctionInfo]] = {
    reader.first match {
      case OVER() =>
        windowFunctionParams(reader.rest).map { wfp =>
          Some(wfp)
        }
      case _ =>
        reader.addAlternates(OVER_SET)
        ParseResult(reader, None)
    }
  }

  private def parseFunctionFilter(reader: Reader): ParseResult[Option[Expression]] = {
    reader.first match {
      case tokens.FILTER() =>
        reader.rest.first match {
          case LPAREN() =>
            val r@ParseResult(r2, _) = where(reader.rest.rest)
            r2.first match {
              case RPAREN() =>
                r.copy(reader = r2.rest)
              case _ =>
                fail(r2, RPAREN())
            }
          case _ =>
            fail(reader.rest, LPAREN())
        }
      case _ =>
        reader.addAlternates(FILTER_SET)
        ParseResult(reader, None)
    }
  }

  // param("var") or param(@table, "var")
  private def param(reader: Reader, pos: Position): ParseResult[Expression] = {
    reader.first match {
      case TableIdentifier(ti) =>
        reader.rest.first match {
          case COMMA() =>
            reader.rest.rest.first match {
              case tokens.StringLiteral(varName) =>
                reader.rest.rest.rest.first match {
                  case RPAREN() =>
                    ParseResult(reader.rest.rest.rest.rest, Hole.SavedQuery(HoleName(varName), ti.drop(1))(pos))
                  case _ =>
                    fail(reader.rest.rest.rest, RPAREN())
                }
              case _ =>
                fail(reader.rest.rest, AStringLiteral)
            }
          case _ =>
            fail(reader.rest, COMMA())
        }
      case _ =>
        fail(reader, ATableIdentifier)
    }
  }

  private def identifierOrFuncall(reader: Reader, ident: Identifier): ParseResult[Expression] = {
    // we want to match
    //    id
    //    id(DISTINCT expr)
    //    id(*) [FILTER(WHERE filterstuff)] [OVER windowstuff]
    //    id(expr,...) [FILTER(WHERE filterstuff)] [OVER windowstuff]
    reader.first match {
      case LPAREN() =>
        val pr@ParseResult(r1, fc) = reader.rest.first match {
          case DISTINCT() =>
            val ParseResult(r2, arg) = nestedExpr(reader.rest.rest)
            r2.first match {
              case RPAREN() =>
                // eww
                ParseResult(r2.rest, FunctionCall(FunctionName(ident.value.toLowerCase + "_distinct"), Seq(arg), None)(ident.pos, ident.pos))
              case _ =>
                fail(reader.rest.rest, RPAREN())
            }
          case STAR() =>
            reader.rest.rest.first match {
              case RPAREN() =>
                ParseResult(reader.rest.rest.rest, FunctionCall(SpecialFunctions.StarFunc(ident.value), Nil, None)(ident.position, ident.position))
              case _ =>
                fail(reader.rest.rest, RPAREN())
            }
          case RPAREN() =>
            ParseResult(reader.rest.rest, FunctionCall(FunctionName(ident.value), Nil, None)(ident.position, ident.position))
          case _ =>
            val ParseResult(r2, args) = parseArgList(reader.rest)
            ParseResult(r2, FunctionCall(FunctionName(ident.value), args, None)(ident.position, ident.position))
        }

        val ParseResult(r2, filterExpr) = parseFunctionFilter(r1)
        val ParseResult(r3, wfi) = parseFunctionOver(r2)
        if (filterExpr.isEmpty && wfi.isEmpty) {
          pr
        } else {
          ParseResult(r3, FunctionCall(fc.functionName, fc.parameters, filterExpr, wfi)(fc.position, ident.position))
        }
      case _ =>
        reader.addAlternates(LPAREN_SET)
        ParseResult(reader, ColumnOrAliasRef(None, ColumnName(ident.value))(ident.position))
    }
  }

  protected def intoQualifier(t: TableIdentifier): String =
    TableName.SodaFountainPrefix + t.value.substring(1) /* remove prefix @ */

  private def joinedColumn(reader: Reader, tableIdent: TableIdentifier): ParseResult[Expression] = {
    reader.first match {
      case DOT() =>
        reader.rest.first match {
          case si@SystemIdentifier(s, _) =>
            ParseResult(reader.rest.rest, ColumnOrAliasRef(Some(intoQualifier(tableIdent)), ColumnName(s))(tableIdent.position))
          case i@Identifier(s, _) =>
            ParseResult(reader.rest.rest, ColumnOrAliasRef(Some(intoQualifier(tableIdent)), ColumnName(s))(tableIdent.position))
          case _ =>
            fail(reader.rest, AnIdentifier)
        }
      case _ =>
        fail(reader, DOT())
    }
  }

  // Ok so after ALL THAT we're finally almost at the bottom!  This is
  // the thing I've arbitrarily decided is "atomic", on the grounds
  // that it is not self-recursive save by going all the way back up
  // to `expr`.  This is where it'd be nicest to be able to use the
  // parser-generator...
  @tailrec
  private def atom(reader: Reader, allowCase: Boolean = true): ParseResult[Expression] = {
    reader.first match {
      case c@CASE() if allowCase =>
        // just because we've seen a CASE doesn't mean we're in a case
        // expression; the user could be referring to a column called
        // "case" or using the case function.
        reader.rest.first match {
          case WHEN() =>
            // ok, this is enough to commit to it
            conditional(reader.rest, c.position)
          case _ =>
            atom(reader, allowCase = false)
        }
      case literal: LiteralToken =>
        val exprified =
          literal match {
            case n: tokens.NumberLiteral => ast.NumberLiteral(n.value)(n.position)
            case s: tokens.StringLiteral => ast.StringLiteral(s.value)(s.position)
            case b: tokens.BooleanLiteral => ast.BooleanLiteral(b.value)(b.position)
            case n: NULL => ast.NullLiteral()(n.position)
          }
        ParseResult(reader.rest, exprified)
      case open@LPAREN() =>
        val ParseResult(r2, e) = nestedExpr(reader.rest)
        r2.first match {
          case RPAREN() =>
            ParseResult(r2.rest, FunctionCall(SpecialFunctions.Parens, Seq(e), None)(open.position, open.position))
          case _ =>
            fail(r2, RPAREN())
        }

      // and this is the place where we diverge the greatest from the
      // original parser.  This would accept all kinds of nonsense like
      // `@gfdsg.:gsdfg(DISTINCT bleh)` and then mangle that into
      // `gsdfg_distinct(bleh)`
      case ident@Identifier("param", false) if allowParamSpecialForms && reader.rest.first == LPAREN() =>
        param(reader.rest.rest, ident.pos)
      case ident: Identifier =>
        identifierOrFuncall(reader.rest, ident)
      case ident: SystemIdentifier =>
        ParseResult(reader.rest, ColumnOrAliasRef(None, ColumnName(ident.value))(ident.position))
      case table: TableIdentifier if allowJoins =>
        joinedColumn(reader.rest, table)

      case hole: HoleIdentifier if allowHoles =>
        ParseResult(reader.rest, Hole.UDF(HoleName(hole.value))(hole.position))

      case _ =>
        reader.resetAlternates()
        fail(reader, AnExpression)
    }
  }

  private def dereference(reader: Reader): ParseResult[Expression] = {
    val ParseResult(r2, e) = atom(reader)
    dereference_!(r2, e)
  }

  @tailrec
  private def dereference_!(reader: Reader, arg: Expression): ParseResult[Expression] = {
    reader.first match {
      case dot@DOT() =>
        val ParseResult(r2, (ident, identPos)) = simpleIdentifier(reader.rest)
        dereference_!(r2, FunctionCall(SpecialFunctions.Subscript, Seq(arg, ast.StringLiteral(ident)(identPos)), None)(arg.position, dot.position))
      case open@LBRACKET() =>
        val ParseResult(r2, index) = nestedExpr(reader.rest)
        r2.first match {
          case RBRACKET() =>
            dereference_!(r2.rest, FunctionCall(SpecialFunctions.Subscript, Seq(arg, index), None)(arg.position, open.position))
          case _ =>
            r2.resetAlternates()
            fail(r2, RBRACKET())
        }
      case _ =>
        reader.addAlternates(DOT_LBRACKET_SET)
        ParseResult(reader, arg)
    }
  }

  private def cast(reader: Reader): ParseResult[Expression] = {
    val ParseResult(r2, arg) = dereference(reader)
    cast_!(r2, arg)
  }

  @tailrec
  private def cast_!(reader: Reader, arg: Expression): ParseResult[Expression] = {
    reader.first match {
      case op@COLONCOLON() =>
        val ParseResult(r2, (ident, identPos)) = simpleIdentifier(reader.rest)
        cast_!(r2, FunctionCall(SpecialFunctions.Cast(TypeName(ident)), Seq(arg), None)(arg.position, identPos))
      case _ =>
        reader.addAlternates(COLONCOLON_SET)
        ParseResult(reader, arg)
    }
  }

  private def unary(reader: Reader): ParseResult[Expression] = {
    reader.first match {
      case op@(MINUS() | PLUS()) =>
        unary(reader.rest).map { arg =>
          FunctionCall(SpecialFunctions.Operator(op.printable), Seq(arg), None)(op.position, op.position)
        }
      case _ =>
        reader.addAlternates(MINUS_PLUS_SET)
        cast(reader)
    }
  }

  private def exp(reader: Reader): ParseResult[Expression] = {
    val ParseResult(r2, arg) = unary(reader)
    r2.first match {
      case op@CARET() =>
        exp(r2.rest).map { arg2 =>
          FunctionCall(SpecialFunctions.Operator(op.printable), Seq(arg, arg2), None)(arg.position, op.position)
        }
      case _ =>
        reader.addAlternates(OPERATOR_SET) // CARETSET - See note in order_!()
        ParseResult(r2, arg)
    }
  }

  private def factor(reader: Reader): ParseResult[Expression] = {
    val ParseResult(r2, e) = exp(reader)
    factor_!(r2, e)
  }

  @tailrec
  private def factor_!(reader: Reader, arg: Expression): ParseResult[Expression] = {
    reader.first match {
      case op@(STAR() | SLASH() | PERCENT()) =>
        val ParseResult(r2, arg2) = exp(reader.rest)
        factor_!(r2, FunctionCall(SpecialFunctions.Operator(op.printable), Seq(arg, arg2), None)(arg.position, op.position))
      case _ =>
        reader.addAlternates(OPERATOR_SET) // FACTORSET - See note in order_!()
        ParseResult(reader, arg)
    }
  }

  private def term(reader: Reader): ParseResult[Expression] = {
    val ParseResult(r2, e) = factor(reader)
    term_!(r2, e)
  }

  @tailrec
  private def term_!(reader: Reader, arg: Expression): ParseResult[Expression] = {
    reader.first match {
      case op@(PLUS() | MINUS() | PIPEPIPE()) =>
        val ParseResult(r2, arg2) = factor(reader.rest)
        term_!(r2, FunctionCall(SpecialFunctions.Operator(op.printable), Seq(arg, arg2), None)(arg.position, op.position))
      case _ =>
        reader.addAlternates(OPERATOR_SET) // TERMSET - See note in order_!()
        ParseResult(reader, arg)
    }
  }

  private def order(reader: Reader): ParseResult[Expression] = {
    val ParseResult(r2, e) = term(reader)
    order_!(r2, e)
  }

  @tailrec
  private def order_!(reader: Reader, arg: Expression): ParseResult[Expression] = {
    reader.first match {
      case op@(EQUALS() | LESSGREATER() | LESSTHAN() | LESSTHANOREQUALS() | GREATERTHAN() | GREATERTHANOREQUALS() | EQUALSEQUALS() | BANGEQUALS()) =>
        val ParseResult(r2, arg2) = term(reader.rest)
        order_!(r2, FunctionCall(SpecialFunctions.Operator(op.printable), Seq(arg, arg2), None)(arg.position, op.position))
      case _ =>
        // So a bunch this adds an expectation of just "an operator"
        // rather than specifying the operators it expects, and the
        // functions above up through `exp` do not.  This only works
        // because these handful of functions are a unique chain of
        // parser functions - everything that enters enter order()
        // (and _only_ those things) will be able to pass through
        // those, and they pass through them without any intervening
        // parsing steps.  As a result, we can just collapse _all_ of
        // their expectations into just "we want some operator here".

        reader.addAlternates(OPERATOR_SET) // COMPARISONSET
        ParseResult(reader, arg)
    }
  }

  private def parseArgList(reader: Reader, arg0: Option[Expression] = None): ParseResult[Seq[Expression]] = {
    reader.first match {
      case RPAREN() =>
        ParseResult(reader.rest, Nil)
      case _ =>
        val args = Vector.newBuilder[Expression]
        args ++= arg0

        @tailrec
        def loop(reader: Reader): Reader = {
          val ParseResult(r2, candidate) = nestedExpr(reader)
          args += candidate
          r2.first match {
            case RPAREN() =>
              r2.rest
            case COMMA() =>
              loop(r2.rest)
            case _ =>
              fail(r2, RPAREN(), COMMA())
          }
        }

        val finalReader = loop(reader)

        ParseResult(finalReader, args.result())
    }
  }

  private def parseIn(name: FunctionName, scrutinee: Expression, op: Token, reader: Reader): ParseResult[Expression] = {
    reader.first match {
      case LPAREN() =>
        parseArgList(reader.rest, arg0 = Some(scrutinee)).map { args =>
          FunctionCall(name, args, None)(scrutinee.position, op.position)
        }
      case _ =>
        fail(reader, LPAREN())
    }
  }

  private def parseLike(name: FunctionName, scrutinee: Expression, op: Token, reader: Reader): ParseResult[Expression] = {
    likeBetweenIn(reader).map { pattern =>
      FunctionCall(name, Seq(scrutinee, pattern), None)(scrutinee.position, op.position)
    }
  }

  private def parseBetween(name: FunctionName, scrutinee: Expression, op: Token, reader: Reader): ParseResult[Expression] = {
    val ParseResult(r2, lowerBound) = likeBetweenIn(reader)
    r2.first match {
      case AND() =>
        likeBetweenIn(r2.rest).map { upperBound =>
          FunctionCall(name, Seq(scrutinee, lowerBound, upperBound), None)(scrutinee.position, op.position)
        }
      case _ =>
        r2.resetAlternates()
        fail(r2, AND())
    }
  }

  // This is an annoying bit of grammar.  This node is defined as
  //   likeBetweenIn [bunchofcases] | order
  // so we need to rewrite it to eliminate the left-recursion.
  // Again, it becomes
  //    lBI = order lBI'
  //    lBI' = [the various cases] lBI' | ε
  private def likeBetweenIn(reader: Reader): ParseResult[Expression] = {
    val ParseResult(r2, arg) = order(reader)
    likeBetweenIn_!(r2, arg)
  }

  @tailrec
  private def likeBetweenIn_!(reader: Reader, arg: Expression): ParseResult[Expression] = {
    reader.first match {
      case is: IS =>
        // This is either IS NULL or IS NOT NULL
        val ParseResult(r2, arg2) =
          reader.rest.first match {
            case NULL() =>
              ParseResult(reader.rest.rest, FunctionCall(SpecialFunctions.IsNull, Seq(arg), None)(arg.position, is.position))
            case NOT() =>
              reader.rest.rest.first match {
                case NULL() =>
                  ParseResult(reader.rest.rest.rest, FunctionCall(SpecialFunctions.IsNotNull, Seq(arg), None)(arg.position, is.position))
                case _ =>
                  fail(reader.rest.rest, NULL())
              }
            case _ =>
              fail(reader.rest, NULL(), NOT())
          }
        likeBetweenIn_!(r2, arg2)
      case like: LIKE =>
        // woo only one possibility!
        val ParseResult(r2, arg2) = parseLike(SpecialFunctions.Like, arg, like, reader.rest)
        likeBetweenIn_!(r2, arg2)
      case between: BETWEEN =>
        val ParseResult(r2, arg2) = parseBetween(SpecialFunctions.Between, arg, between, reader.rest)
        likeBetweenIn_!(r2, arg2)
      case not: NOT =>
        // could be NOT BETWEEN, NOT IN, or NOT LIKE
        reader.rest.first match {
          case between: BETWEEN =>
            val ParseResult(r2, arg2) = parseBetween(SpecialFunctions.NotBetween, arg, not, reader.rest.rest)
            likeBetweenIn_!(r2, arg2)
          case in: IN =>
            val ParseResult(r2, arg2) = parseIn(SpecialFunctions.NotIn, arg, not, reader.rest.rest)
            likeBetweenIn_!(r2, arg2)
          case like: LIKE =>
            val ParseResult(r2, arg2) = parseLike(SpecialFunctions.NotLike, arg, not, reader.rest.rest)
            likeBetweenIn_!(r2, arg2)
          case other =>
            fail(reader.rest, BETWEEN(), IN(), LIKE())
        }
      case in: IN =>
        // Again only one possibility!
        val ParseResult(r2, arg2) = parseIn(SpecialFunctions.In, arg, in, reader.rest)
        likeBetweenIn_!(r2, arg2)
      case _ =>
        reader.addAlternates(LIKE_BETWEEN_IN_SET)
        ParseResult(reader, arg)
    }
  }

  private def negation(reader: Reader): ParseResult[Expression] = {
    reader.first match {
      case op: NOT =>
        negation(reader.rest).map { arg =>
          FunctionCall(SpecialFunctions.Operator(op.printable), Seq(arg), None)(op.position, op.position)
        }
      case _ =>
        reader.addAlternates(NOT_SET)
        likeBetweenIn(reader)
    }
  }

  private def conjunction(reader: Reader): ParseResult[Expression] = {
    val ParseResult(r2, e) = negation(reader)
    conjunction_!(r2, e)
  }

  @tailrec
  private def conjunction_!(reader: Reader, arg: Expression): ParseResult[Expression] = {
    reader.first match {
      case and: AND =>
        val ParseResult(r2, arg2) = negation(reader.rest)
        conjunction_!(r2, FunctionCall(SpecialFunctions.Operator(and.printable), Seq(arg, arg2), None)(arg.position, and.pos))
      case _ =>
        reader.addAlternates(AND_SET)
        ParseResult(reader, arg)
    }
  }

  // This is a pattern that'll be used all over this parser.  The
  // combinators approach was
  //    disjunction = (disjunction `OR` conjunction) | conjunction;
  // which is left-recursive.  We can eliminate that by instead doing
  //    disjunction = conjunction disjunction'
  //    disjunction' = `OR` conjunction disjunction' | ε
  private def disjunction(reader: Reader): ParseResult[Expression] = {
    val ParseResult(r2, e) = conjunction(reader)
    disjunction_!(r2, e)
  }

  @tailrec
  private def disjunction_!(reader: Reader, arg: Expression): ParseResult[Expression] = {
    reader.first match {
      case or: OR =>
        val ParseResult(r2, arg2) = conjunction(reader.rest)
        disjunction_!(r2, FunctionCall(SpecialFunctions.Operator(or.printable), Seq(arg, arg2), None)(arg.position, or.pos))
      case _ =>
        reader.addAlternates(OR_SET)
        ParseResult(reader, arg)
    }
  }

  protected final def nestedExpr(reader: Reader): ParseResult[Expression] = {
    disjunction(reader)
  }

  protected def topLevelExpr(reader: Reader): ParseResult[Expression] = {
    val r = disjunction(reader)
    r.reader.resetAlternates()
    r
  }

  // The remainder of these parsers aren't used here but may be useful
  // to subclasses that extend the soql language in some way.  They
  // all assume that it is certain the thing being parsed is desired
  // and will fail if it is not present.

  protected final def qualifiedSystemIdentifier(reader: Reader): ParseResult[(String, ColumnName, Position)] = {
    reader.first match {
      case ti: TableIdentifier =>
        reader.rest.first match {
          case DOT() =>
            reader.rest.rest.first match {
              case si: SystemIdentifier =>
                ParseResult(reader.rest.rest.rest, (intoQualifier(ti), ColumnName(si.value), ti.position))
              case _ =>
                fail(reader.rest.rest, ASystemIdentifier)
            }
          case _ =>
            fail(reader.rest, DOT())
        }
      case _ =>
        fail(reader, AQualifiedSystemIdentifier)
    }
  }

  protected final def possiblyQualifiedSystemIdentifier(reader: Reader): ParseResult[(Option[String], ColumnName, Position)] = {
    reader.first match {
      case ti: TableIdentifier =>
        qualifiedSystemIdentifier(reader).map { case (qual, cn, pos) =>
          (Some(qual), cn, pos)
        }
      case si: SystemIdentifier =>
        ParseResult(reader.rest, (None, ColumnName(si.value), si.position))
      case _ =>
        fail(reader, ATableIdentifier, ASystemIdentifier)
    }
  }

  protected final def qualifiedUserIdentifier(reader: Reader): ParseResult[(String, ColumnName, Position)] = {
    reader.first match {
      case ti: TableIdentifier =>
        reader.rest.first match {
          case DOT() =>
            reader.rest.rest.first match {
              case ui: Identifier =>
                ParseResult(reader.rest.rest.rest, (intoQualifier(ti), ColumnName(ui.value), ti.position))
              case _ : SystemIdentifier =>
                fail(reader.rest.rest, ANonSystemIdentifier)
              case _ =>
                fail(reader.rest.rest, AnIdentifier)
            }
          case _ =>
            fail(reader.rest, DOT())
        }
      case _ =>
        fail(reader, AQualifiedIdentifier)
    }
  }

  protected final def possiblyQualifiedUserIdentifier(reader: Reader): ParseResult[(Option[String], ColumnName, Position)] = {
    reader.first match {
      case ti: TableIdentifier =>
        qualifiedUserIdentifier(reader).map { case (qual, cn, pos) =>
          (Some(qual), cn, pos)
        }
      case ui: Identifier =>
        ParseResult(reader.rest, (None, ColumnName(ui.value), ui.position))
      case si: SystemIdentifier =>
        fail(reader, ANonSystemIdentifier)
      case _ =>
        fail(reader, ATableIdentifier, AnIdentifier)
    }
  }

  protected final def qualifiedIdentifier(reader: Reader): ParseResult[(String, ColumnName, Position)] = {
    reader.first match {
      case ti: TableIdentifier =>
        reader.rest.first match {
          case DOT() =>
            reader.rest.rest.first match {
              case ui: Identifier =>
                ParseResult(reader.rest.rest.rest, (intoQualifier(ti), ColumnName(ui.value), ti.position))
              case si : SystemIdentifier =>
                ParseResult(reader.rest.rest.rest, (intoQualifier(ti), ColumnName(si.value), ti.position))
              case _ =>
                fail(reader.rest.rest, AnIdentifier)
            }
          case _ =>
            fail(reader.rest, DOT())
        }
      case _ =>
        fail(reader, AQualifiedIdentifier)
    }
  }

  protected final def possiblyQualifiedIdentifier(reader: Reader): ParseResult[(Option[String], ColumnName, Position)] = {
    reader.first match {
      case ti: TableIdentifier =>
        qualifiedIdentifier(reader).map { case (qual, cn, pos) =>
          (Some(qual), cn, pos)
        }
      case ui: Identifier =>
        ParseResult(reader.rest, (None, ColumnName(ui.value), ui.position))
      case si: SystemIdentifier =>
        ParseResult(reader.rest, (None, ColumnName(si.value), si.position))
      case _ =>
        fail(reader, ATableIdentifier, AnIdentifier)
    }
  }
}
