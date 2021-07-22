import org.rogach.scallop._

import scala.io.Source
import java.io.{InputStream, PrintWriter}

import lex.Lexer
import parse.Parser
import parse.Translator
import genConstraints.genBasic


object Main {
  private class MainConf(args: Seq[String]) extends ScallopConf(args) {
    val infile = opt[String](required = true)
    val outfile = opt[String](default = Some("out.z3"))
    val norun = opt[Boolean](default = Some(false))
    val timeout = opt[Int](default = Some(60))

    verify()
  }

  def getStringfromStream(s: InputStream): String = {
    val barray: Array[Byte] = Array.fill[Byte](1024)(0)
    s.read(barray);
    barray.map(_.toChar).mkString
  }

  def callZ3(fName: String, timeout: Int): Unit = {
    val cmdString = s"z3 $fName"
    val p: Process = Runtime.getRuntime.exec(cmdString)
    if (!p.waitFor(timeout, java.util.concurrent.TimeUnit.SECONDS))
      utils.printMagenta(s"Timeout occured after $timeout seconds")
    else utils.printGreen(getStringfromStream(p.getInputStream))
  }

  def main(args: Array[String]): Unit = {
    val conf = new MainConf(args.toIndexedSeq)
    val ifName = conf.infile()
    val ofName = conf.outfile()
    val tokens = Lexer.tokenize(Source.fromFile(ifName).toList)
    val semgusFile = Translator.parseSemgusFile(Parser.parse(tokens))
    val SMTConsts = genBasic.semgus2SMT(semgusFile.purify)
    new PrintWriter(ofName){write(SMTConsts.mkString("\n")); close}
    if (conf.norun()) () else callZ3(ofName, conf.timeout())
  }
}
