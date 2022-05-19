import lang.Semgus._
import org.semgus.java.event.MetaSpecEvent._
import org.semgus.java.event.SemgusSpecEvent.DefineTermTypeEvent.Constructor
import org.semgus.java.event.SemgusSpecEvent.SynthFunEvent.NonTerminal
import org.semgus.java.event.SemgusSpecEvent._
import org.semgus.java.event._

import scala.jdk.CollectionConverters._

class ExternalParseException(message: String) extends Exception(message)

package object semgusJava {

  def readJSONfromFile(fname: String): String = {
    val file = scala.io.Source.fromFile(fname)
    val jsonstr = file.mkString("\n")
    file.close(); jsonstr
  }

  def parseSemgusFile(fname: String): List[SpecEvent] = EventParser.parse(readJSONfromFile(fname)).asScala.toList

  private def relationAppStr(r: org.semgus.java.`object`.RelationApp): String = {
    val relName = r.name
    val relArgs = r.arguments.asScala.mkString(" ")
    s"($relName $relArgs)"
  }

  def translateRHSConstructors(const: Constructor): RHSExp =
    if (const.children().isEmpty) RHSLeaf(const.name)
    else RHSOp(const.name, const.children.asScala.toList.map{x => RHSNT(NonTerminal(x, x))})

  def translateNTdef(d: DefineTermTypeEvent): LHSProductionSet = {
    val NTLHS = LHS(NonTerminal(d.name(), d.name()))
    val NTRHSList = d.constructors().asScala.toList.map { c => RHS(translateRHSConstructors(c)) }
    LHSProductionSet(NTLHS, NTRHSList)
  }

  def translateCHC(h: HornClauseEvent): SemanticCHC = {
    val headRels = relationAppStr(h.head)
    val premiseRels = h.bodyRelations.asScala.map{relationAppStr}.mkString(" ")
    val constraint = h.constraint.toString
    val premise = s"(and $premiseRels $constraint)"
    SemanticCHC(SMTFormula(headRels), SMTFormula(premise))
  }

  def translateConstraint(c: ConstraintEvent): Constraint = Constraint(SMTFormula(c.constraint.toString))

  def translateSynthGrammar(grm: Map[String, NonTerminal]): List[LHSProductionSet] = grm.map{
    case (ntName, ntdef) =>
      val RHSes = ntdef.productions.asScala.map{
        case (_, prod) => RHS(if (prod.occurrences.isEmpty) RHSLeaf(prod.operator) else
          RHSOp(prod.operator, prod.occurrences.asScala.toList.map{x => RHSNT(NonTerminal(x, grm(x).termType))}))
      }.toList
      LHSProductionSet(LHS(NonTerminal(ntName, ntdef.termType)), RHSes)
  }.toList

  def translateSynthFunEvent(s: SynthFunEvent): SynthFun = {
    val fName = s.name
    val fNTType = s.termType
    val fGrammar = translateSynthGrammar(s.grammar.asScala.toMap)
    SynthFun(fName, fNTType, fGrammar)
  }

  def translateEvent(event: SpecEvent): Option[SemgusElement] = {
    event match {
      case _: CheckSynthEvent => None
      case _: DeclareTermTypeEvent => None
      case d: DefineTermTypeEvent => Some(translateNTdef(d))
      case h: HornClauseEvent => Some(translateCHC(h))
      case c: ConstraintEvent => Some(translateConstraint(c))
      case s: SynthFunEvent => Some(translateSynthFunEvent(s))
      case _: SetInfoEvent => None
      case _: StreamEndEvent => None
      case _ => throw new ExternalParseException("Unexpected event supplied by external JSON parser")
    }
  }

  def translate2Semgus(events: List[SpecEvent]): SemgusFile = SemgusFile(utils.filterNones(events.map{translateEvent}))
}
