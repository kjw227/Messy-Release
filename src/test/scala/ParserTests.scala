import org.junit.Test
import lex.Lexer
import parse.Parser
import parse.Translator

import scala.io.Source

class ParserTestSuite{
  def exampleTest: Unit = {
    val tokens = Lexer.tokenize(Source.fromFile("./resources/example.sem").toList)
    val sexpr = Parser.parse(tokens)
    print(sexpr)
  }
}

class TranslatorTestSuite{
  @Test
  def exampleTest: Unit = {
    val tokens = Lexer.tokenize(Source.fromFile("./resources/example.sem").toList)
    val semgusFileStr = Translator.parseSemgusFile(Parser.parse(tokens)).toString
    val reTokenized = Lexer.tokenize(semgusFileStr.toList)
    val reParsed = Translator.parseSemgusFile(Parser.parse(reTokenized)).toString
    println(reParsed == semgusFileStr)
  }
}
