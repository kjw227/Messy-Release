package lang
package Semgus

sealed trait Semgus {
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

sealed trait Command

sealed trait Declaration extends Semgus with Command
case class Metadata(tag: String, data: Semgus) extends Semgus with Command

case class SortDeclaration(sortName: Sort) extends Declaration
case class TermDeclaration(sortName: Sort) extends Declaration
case class VarDeclaration(varName: String, sortName: Sort) extends Declaration
case class RelDeclaration(relName: String, args: List[Sort]) extends Declaration
case class NTDeclaration(ntName: String, ntType: Sort, ntRel: RelDeclaration) extends Declaration

case class SynthBlock(name: String, termType: Sort, nts: List[NTDeclaration],
                      vars: List[VarDeclaration], prods: List[ProductionSet]) extends Semgus with Command

case class ProductionSet(lhs: LHS, rhsList: List[RHS]) extends Semgus with Command
case class LHS(ntName: String, ntTerm: String, ntRel: SMTFormula) extends Semgus
case class RHS(rhsExp: RHSExp, premises: List[SMTFormula]) extends Semgus

sealed trait RHSExp extends Semgus
sealed trait RHSAtom extends RHSExp

case class RHSOp(opName: String, args: List[RHSAtom]) extends RHSExp
case class RHSNT(ntName: String, ntTerm: String) extends RHSAtom
case class RHSLeaf(leafName: String) extends RHSAtom

case class SMTFormula(formula: String) extends Semgus

case class Constraint(formula: SMTFormula) extends Semgus with Command

case class SemgusFile(commands: List[Command]) {
  override def toString: String = commands.mkString("\n")
}
