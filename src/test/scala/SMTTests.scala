import org.junit.Test
import lex.Lexer
import parse.Parser
import parse.Translator
import genConstraints.genBasic

import scala.io.Source

class SMTTestSuite {
  @Test
  def SMTTest: Unit = {
    val tokens = Lexer.tokenize(Source.fromFile("./resources/example.sem").toList)
    val semgusFile = Translator.parseSemgusFile(Parser.parse(tokens))
    val consts = genBasic.semgus2SMT(semgusFile)
    println(consts.mkString("\n"))
  }
}
