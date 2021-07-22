package lex

import lang.Token._

object Lexer {
  private val parens = Set(')', '(')
  private val whitespace = Set(' ', '\n', '\r', '\t')
  private val quotes = Set('"')

  @scala.annotation.tailrec
  def recursiveTokenize(input: List[Char], currentStr: String, tokenList: List[Token]): List[Token] = {
    input match {
      case c::remains =>
        if (c == '(') recursiveTokenize(remains, "", LParen::StrToken(currentStr)::tokenList)
        else if (c == ')') recursiveTokenize(remains, "", RParen::StrToken(currentStr)::tokenList)
        else if (c == '"') recursiveTokenize(remains, "", Quote::StrToken(currentStr)::tokenList)
        else if (c == ' ' || c == '\t')
          recursiveTokenize(remains, "", Whitespace::StrToken(currentStr)::tokenList)
        else if (c == '\n' || c == '\r')
          recursiveTokenize(remains, "", Newline::StrToken(currentStr)::tokenList)
        else if (c == ';') recursiveTokenize(remains, "", SemiColon::StrToken(currentStr)::tokenList)
        else recursiveTokenize(remains, currentStr.appended(c), tokenList)
      case Nil => tokenList.reverse
    }
  }
  def tokenize(input: List[Char]): List[Token] = recursiveTokenize(input, "", Nil).filter(t => !t.isEmpty)
}
