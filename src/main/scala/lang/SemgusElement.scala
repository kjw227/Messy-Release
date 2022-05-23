package lang
package Semgus

sealed trait SemgusElement {
  protected def purifyName(name: String): String = if (name.startsWith("0")) s"purified$name" else name
  def purify: SemgusElement

  override def toString: String = this match {
    case SortDeclaration(sortName) => s"(declare-sort $sortName)"
    case VarDeclaration(varName, sortName) => s"(declare-var $varName $sortName)"
    case RelDeclaration(relName, args) => val argStr = args.mkString(" "); s"(declare-rel $relName $argStr)"
    case NTDeclaration(ntName, ntType, ntRel) =>
      val relName = ntRel.relName
      val relArgs = ntRel.args.mkString(" ")
      val relStr = s"($relName ($relArgs))"
      s"(declare-nt $ntName $ntType $relStr)"
    case SynthFun(name, tp, grm) =>
      val grmStr = grm.mkString("\n")
      s"(synth-term $name $tp\n($grmStr)\n)\n"
    case LHSProductionSet(lhs, rhsList) => val rhsStr = rhsList.mkString("\n"); s"($lhs\n$rhsStr)"
    case Nonterminal(ntName, ntType) => s"(nonterminal $ntName $ntType)"
    case LHS(nt) => s"($nt)"
    case RHS(rhsExp) => s"($rhsExp)"
    case RHSOp(opName, args) => val argStr = args.mkString(" "); s"($opName $argStr)"
    case RHSNT(ntName) => s"$ntName"
    case RHSLeaf(leafName) => leafName
    case SemanticCHC(decl, vars, head, tail) => s"(rule ($head) ($tail))"
    case Variable(name, tp) => s""
    case SMTFormula(formula) => formula
    case Constraint(formula) => s"(constraint $formula)"
  }
}

sealed trait SemgusEvent extends SemgusElement{
  override def purify: SemgusEvent
}

sealed trait Declaration extends SemgusEvent
case class SortDeclaration(sortName: Sort) extends Declaration {
  override def purify: SortDeclaration = SortDeclaration(purifyName(sortName))
}
case class VarDeclaration(varName: String, sortName: Sort) extends Declaration {
  override def purify: VarDeclaration = VarDeclaration(purifyName(varName), purifyName(sortName))
}
case class RelDeclaration(relName: String, args: List[Sort]) extends Declaration {
  override def purify: RelDeclaration = RelDeclaration(purifyName(relName), args.map{purifyName})
}
case class NTDeclaration(ntName: String, ntType: Sort, ntRel: RelDeclaration) extends Declaration {
  override def purify: NTDeclaration = NTDeclaration(purifyName(ntName), purifyName(ntType), ntRel.purify)
}

case class SynthFun(name: String, termType: Sort, grm: List[LHSProductionSet]) extends SemgusEvent {
  override def purify: SynthFun =
    SynthFun(purifyName(name), purifyName(termType), grm.map{_.purify})
}


case class LHSProductionSet(lhs: LHS, rhsList: List[RHS]) extends SemgusEvent {
  override def purify: LHSProductionSet = LHSProductionSet(lhs.purify, rhsList.map{_.purify})
}
case class Nonterminal(ntName: String, ntType: Sort) extends SemgusElement {
  override def purify: Nonterminal = Nonterminal(purifyName(ntName), purifyName(ntType))
}
case class LHS(nt: Nonterminal) extends SemgusElement {
  override def purify: LHS = LHS(nt.purify)
}
case class RHS(rhsExp: RHSExp) extends SemgusElement {
  override def purify: RHS = RHS(rhsExp.purify)
}
sealed trait RHSExp extends SemgusElement {
  override def purify: RHSExp
}
sealed trait RHSAtom extends RHSExp {
  override def purify: RHSAtom
}
case class RHSOp(opName: String, args: List[RHSAtom]) extends RHSExp {
  override def purify: RHSOp = RHSOp(purifyName(opName), args.map{_.purify})
}
case class RHSNT(nt: Nonterminal) extends RHSAtom {
  override def purify: RHSNT = RHSNT(nt.purify)
}
case class RHSLeaf(leafName: String) extends RHSAtom {
  override def purify: RHSLeaf = RHSLeaf(purifyName(leafName))
}


case class SMTFormula(formula: String) extends SemgusElement {
  override def purify: SMTFormula = this
}
case class SemanticCHC(decl: RelDeclaration, vars: Set[Variable], head: SMTFormula, tail: SMTFormula)
  extends SemgusEvent {
  override def purify: SemanticCHC = SemanticCHC(decl.purify, vars.map{_.purify}, head.purify, tail.purify)
}
case class Variable(name: String, tp: Sort) extends SemgusElement {
  override def purify: Variable = Variable(purifyName(name), tp)
}

case class Constraint(formula: SMTFormula) extends SemgusEvent {
  override def purify: Constraint = Constraint(formula.purify)
}

case class SemgusFile(commands: List[SemgusEvent]) {
  def purify: SemgusFile = SemgusFile(commands.map{_.purify})
  override def toString: String = commands.mkString("\n")
}
