package parse

import lang.Token._
import lang.SExpr._

class ParseException(message: String) extends Exception(message)

object Parser {
  private case class ParseResult(result: SExpr, remains: List[Token])

  private def parseNewComment(tokens: List[Token]): List[Token] = tokens.head match {
    case Newline => tokens.tail
    case _ => parseNewComment(tokens.tail)
  }

  private def parseNewString(tokens: List[Token], builtStr: String): ParseResult = tokens.head match {
      case Quote => ParseResult(SExprString(builtStr), tokens.tail)
      case Newline => throw new ParseException("Newlines should not be found within string literals")
      case t: Token => parseNewString(tokens.tail, builtStr + t.toString)
    }

  private def parseNewSExpr(tokens: List[Token], SExprs: List[SExpr]): ParseResult = tokens.head match {
      case LParen => val p = parseNewSExpr(tokens.tail, Nil); parseNewSExpr(p.remains, p.result::SExprs)
      case RParen => ParseResult(SExprList(SExprs.reverse), tokens.tail)
      case Whitespace => parseNewSExpr(tokens.tail, SExprs)
      case Newline => parseNewSExpr(tokens.tail, SExprs)
      case Quote => val p = parseNewString(tokens.tail, ""); parseNewSExpr(p.remains, p.result::SExprs)
      case SemiColon => parseNewSExpr(parseNewComment(tokens.tail), SExprs)
      case StrToken(s) => val t = if(s.forall(_.isDigit)) SExprInt(s.toInt)
        else if (s == "true") SExprBool(true)
        else if (s == "false") SExprBool(false)
        else if (s.take(2) == "#x") SExprBV(Integer.parseInt(s.drop(2), 16))
        else SExprName(s)
        parseNewSExpr(tokens.tail, t::SExprs)
    }

  def parse(tokens: List[Token]): SExpr = parseNewSExpr(tokens.appended(RParen), Nil).result
}
