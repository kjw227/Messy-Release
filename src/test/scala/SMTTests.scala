import org.junit.Test
import lex.Lexer
import parse.Parser
import parse.Translator
import genConstraints.genBasic
import java.io.File

import scala.io.Source

class SMTTestSuite {
  def SMTTest: Unit = {
    val benchmarks = new File("./resources/test_sem_files/").listFiles.filter(_.isFile).toList
    benchmarks.foreach{genSMTFile}
  }

  def genSMTFile(bFile: File): Unit = {
    val fName = bFile.getName + ".smt2"
    val tokens = Lexer.tokenize(Source.fromFile(bFile).toList)
    val semgusFile = Translator.parseSemgusFile(Parser.parse(tokens))
  }
}
