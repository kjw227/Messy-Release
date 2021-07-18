package lex

import lang.Token._

object Lexer {
  private val parens = Set(')', '(')
  private val whitespace = Set(' ', '\n', '\r', '\t')
  private val quotes = Set('"')

  def recursiveTokenize(input: List[Char], currentStr: String): List[Token] = {
    input match {
      case c::remains =>
        if (c == '(') StrToken(currentStr)::LParen::recursiveTokenize(remains, "")
        else if (c == ')') StrToken(currentStr)::RParen::recursiveTokenize(remains, "")
        else if (c == '"') StrToken(currentStr)::Quote::recursiveTokenize(remains, "")
        else if (c == ' ' || c == '\t') StrToken(currentStr)::Whitespace::recursiveTokenize(remains, "")
        else if (c == '\n' || c == '\r') StrToken(currentStr)::Newline::recursiveTokenize(remains, "")
        else if (c == ';') StrToken(currentStr)::SemiColon::recursiveTokenize(remains, "")
        else recursiveTokenize(remains, currentStr.appended(c))
      case Nil => Nil
    }
  }
  def tokenize(input: List[Char]): List[Token] = recursiveTokenize(input, "").filter(t => !t.isEmpty)
}
