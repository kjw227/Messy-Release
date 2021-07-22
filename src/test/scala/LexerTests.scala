import lex.Lexer
import org.junit.Test

import scala.io.Source

class LexerTestSuite{
  def exampleTest = {
    val example = Source.fromFile("./resources/test_sem_files/example.sem").toList
    val tokens = Lexer.tokenize(example)
    print(tokens)
  }
}