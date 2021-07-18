import lex.Lexer
import org.junit.Test

import scala.io.Source

class TestSuite{
  @Test
  def exampleTest = {
    val example = Source.fromFile("./resources/example.sem").toList
    val tokens = Lexer.tokenize(example)
    print(tokens)
  }
}