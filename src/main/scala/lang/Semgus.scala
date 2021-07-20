package lang
package Semgus

sealed trait Semgus {
  protected def purifyName(name: String): String = if (name.startsWith("0")) s"purified$name" else name
  def purify: Semgus

  override def toString: String = this match {
    case Metadata(tag, data) => s"(metadata :$tag $data)"
    case SortDeclaration(sortName) => s"(declare-sort $sortName)"
    case TermDeclaration(sortName) => s"(declare-term-type $sortName)"
    case VarDeclaration(varName, sortName) => s"(declare-var $varName $sortName)"
    case RelDeclaration(relName, args) => val argStr = args.mkString(" "); s"(declare-rel $relName $argStr)"
    case NTDeclaration(ntName, ntType, ntRel) =>
      val relName = ntRel.relName
      val relArgs = ntRel.args.mkString(" ")
      val relStr = s"($relName $relArgs)"
      s"(declare-nt $ntName $ntType $relStr)"
    case SynthBlock(name, tp, nts, vars, prods) =>
      val ntStr = nts.mkString("\n")
      val varStr = vars.mkString("\n")
      val prodStr = prods.mkString("\n")
      s"(synth-term $name $tp\n($ntStr\n$varStr\n$prodStr))"
    case ProductionSet(lhs, rhsList) => val rhsStr = rhsList.mkString("\n"); s"($lhs\n$rhsStr)"
    case LHS(ntName, ntTerm, ntRel) => s"($ntName $ntTerm) $ntRel"
    case RHS(rhsExp, premises) => val premiseStr = premises.mkString("\n"); s"($rhsExp\n$premiseStr)"
    case RHSOp(opName, args) => val argStr = args.mkString(" "); s"($opName $argStr)"
    case RHSNT(ntName, ntTerm) => s"($ntName $ntTerm)"
    case RHSLeaf(leafName) => leafName
    case SMTFormula(formula) => formula
    case Constraint(formula) => s"(constraint $formula)"
  }
}

sealed trait Command extends Semgus {
  override def purify: Command
}

sealed trait Declaration extends Command
case class Metadata(tag: String, data: Semgus) extends Command {
  override def purify: Metadata = this
}
case class SortDeclaration(sortName: Sort) extends Declaration {
  override def purify: SortDeclaration = SortDeclaration(purifyName(sortName))
}
case class TermDeclaration(sortName: Sort) extends Declaration {
  override def purify: TermDeclaration = TermDeclaration(purifyName(sortName))
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
case class SynthBlock(name: String, termType: Sort, nts: List[NTDeclaration],
                      vars: List[VarDeclaration], prods: List[ProductionSet]) extends Command {
  override def purify: SynthBlock =
    SynthBlock(purifyName(name), purifyName(termType), nts.map{_.purify}, vars.map{_.purify}, prods.map{_.purify})
}
case class ProductionSet(lhs: LHS, rhsList: List[RHS]) extends Command {
  override def purify: ProductionSet = ProductionSet(lhs.purify, rhsList.map{_.purify})
}
case class LHS(ntName: String, ntTerm: String, ntRel: SMTFormula) extends Semgus {
  override def purify: LHS = LHS(purifyName(ntName), purifyName(ntTerm), ntRel.purify)
}
case class RHS(rhsExp: RHSExp, premises: List[SMTFormula]) extends Semgus {
  override def purify: RHS = RHS(rhsExp.purify, premises.map{_.purify})
}

sealed trait RHSExp extends Semgus {
  override def purify: RHSExp
}
sealed trait RHSAtom extends RHSExp {
  override def purify: RHSAtom
}

case class RHSOp(opName: String, args: List[RHSAtom]) extends RHSExp {
  override def purify: RHSOp = RHSOp(purifyName(opName), args.map{_.purify})
}
case class RHSNT(ntName: String, ntTerm: String) extends RHSAtom {
  override def purify: RHSNT = RHSNT(purifyName(ntName), purifyName(ntTerm))
}
case class RHSLeaf(leafName: String) extends RHSAtom {
  override def purify: RHSLeaf = RHSLeaf(purifyName(leafName))
}
case class SMTFormula(formula: String) extends Semgus {
  override def purify: SMTFormula = this
}
case class Constraint(formula: SMTFormula) extends Semgus with Command {
  override def purify: Constraint = Constraint(formula.purify)
}

case class SemgusFile(commands: List[Command]) {
  def purify: SemgusFile = SemgusFile(commands.map{_.purify})
  override def toString: String = commands.mkString("\n")
}
