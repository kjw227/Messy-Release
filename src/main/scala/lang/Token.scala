package lang
package Token

sealed trait Token {
  def isEmpty: Boolean = this match {
    case StrToken(s) => s.isEmpty
    case _ => false
  }

  override def toString: String = this match {
    case LParen => "("
    case RParen => ")"
    case Whitespace => " "
    case Newline => "\n"
    case Quote => "\""
    case SemiColon => ";"
    case StrToken(s) => s
  }
}

case object LParen extends Token
case object RParen extends Token
case object Whitespace extends Token
case object Newline extends Token
case object Quote extends Token
case object SemiColon extends Token
case class StrToken(s: String) extends Token
