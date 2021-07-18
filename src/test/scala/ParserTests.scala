import org.junit.Test

import lex.Lexer
import parse.Parser
import scala.io.Source

class ParserTestSuite{
  @Test
  def exampleTest: Unit = {
    val tokens = Lexer.tokenize(Source.fromFile("./resources/example.sem").toList)
    val sexpr = Parser.parse(tokens)
    print(sexpr)
  }
}
