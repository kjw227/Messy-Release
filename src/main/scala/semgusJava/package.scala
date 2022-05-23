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
    val jsonstr = file.mkString("")
    file.close(); utils.printGreen(jsonstr); jsonstr
  }

  def typedVarWithTypeStr(v: org.semgus.java.`object`.TypedVar): String = {
    val vName = v.name
    val vType = v.`type`
    s"(as $vName $vType)"
  }
  def varWithTypeStr(v: org.semgus.java.`object`.SmtTerm.Variable): String = {
    val vName = v.name
    val vType = v.`type`
    s"(as $vName $vType)"
  }

  def constraint2Str(c: org.semgus.java.`object`.SmtTerm): String = c match {
    case a: org.semgus.java.`object`.SmtTerm.Application =>
      if (a.arguments.isEmpty) a.name.toString else {
        val aName = a.name
        val argString = a.arguments.asScala.map{x => constraint2Str(x.term)}.mkString(" ")
        s"($aName $argString)"
      }
    case _: org.semgus.java.`object`.SmtTerm.Quantifier =>
      throw new ExternalParseException("Quantifiers should not be in constraints")
    case v: org.semgus.java.`object`.SmtTerm.Variable => varWithTypeStr(v)
    case rest => rest.toString
  }

  def parseSemgusFile(fname: String): List[SpecEvent] = EventParser.parse(readJSONfromFile(fname)).asScala.toList

  private def relationAppStr(r: org.semgus.java.`object`.RelationApp): String = {
    val relName = r.name
    val relArgs = r.arguments.asScala.map{
      tv => typedVarWithTypeStr(tv)}.mkString(" ")
    if (relArgs.isEmpty) s"$relName" else s"($relName $relArgs)"
  }

  def translateRHSConstructors(const: Constructor): RHSExp =
    if (const.children().isEmpty) RHSLeaf(const.name)
    else RHSOp(const.name, const.children.asScala.toList.map{x => RHSNT(Nonterminal(x, x))})

  def translateNTdef(d: DefineTermTypeEvent): LHSProductionSet = {
    val NTLHS = LHS(Nonterminal(d.name(), d.name()))
    val NTRHSList = d.constructors().asScala.toList.map {c => RHS(translateRHSConstructors(c))}
    LHSProductionSet(NTLHS, NTRHSList)
  }

  def translateCHC(h: HornClauseEvent): SemanticCHC = {
    val decl = RelDeclaration(h.head.name, h.head.arguments.asScala.map{tv => tv.`type`.toString}.toList)
    val headVars = h.head.arguments.asScala.map{
      tv => Variable(tv.name, tv.`type`.toString)}.toSet
    val headRels = relationAppStr(h.head)
    utils.printMagenta(headRels)
    val premiseRels = h.bodyRelations.asScala.map{relationAppStr}.mkString(" ")
    utils.printMagenta(premiseRels)
    val relVars = h.bodyRelations.asScala.foldLeft(Set(): Set[Variable]){
      case (relAcc, rel) => rel.arguments.asScala.foldLeft(Set(): Set[Variable]){
        case (varAcc, v) => varAcc + Variable(v.name, v.`type`.toString)}.union(relAcc)}

    val constraint = constraint2Str(h.constraint)
    utils.printYellow(constraint)
    val premise = s"(and $premiseRels $constraint)"
    SemanticCHC(decl, headVars.union(relVars), SMTFormula(premise), SMTFormula(headRels))
  }

  def translateConstraint(c: ConstraintEvent): Constraint = Constraint(SMTFormula(constraint2Str(c.constraint)))

  def translateSynthGrammar(grm: Map[String, NonTerminal]): List[LHSProductionSet] = grm.map{
    case (ntName, ntdef) =>
      val RHSes = ntdef.productions.asScala.map{
        case (_, prod) => RHS(if (prod.occurrences.isEmpty) RHSLeaf(prod.operator) else
          RHSOp(prod.operator, prod.occurrences.asScala.toList.map{x => RHSNT(Nonterminal(x, grm(x).termType))}))
      }.toList
      LHSProductionSet(LHS(Nonterminal(ntName, ntdef.termType)), RHSes)
  }.toList

  def translateSynthFunEvent(s: SynthFunEvent): SynthFun = {
    val fName = s.name
    val fNTType = s.termType
    val fGrammar = translateSynthGrammar(s.grammar.asScala.toMap)
    SynthFun(fName, fNTType, fGrammar)
  }

  def translateEvent(event: SpecEvent): Option[SemgusEvent] = {
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

  def JSON2Semgus(fname: String): SemgusFile = translate2Semgus(parseSemgusFile(fname))
}
