package lang
package Semgus

sealed trait Semgus

sealed trait Command

sealed trait Declaration extends Semgus with Command
case class Metadata(tag: String, data: Semgus) extends Semgus with Command
case class Constraint(formula: SMTFormula) extends Semgus with Command

case class SortDeclaration(sortName: Sort) extends Declaration
case class TermDeclaration(sortName: Sort) extends Declaration
case class VarDeclaration(varName: String) extends Declaration
case class RelDeclaration(relName: String, args: List[Sort]) extends Declaration
case class NTDeclaration(NTName: String, NTType: Sort, rel: RelDeclaration) extends Declaration

case class SynthBlock(NTs: List[NTDeclaration], vars: List[VarDeclaration]) extends Semgus with Command

case class ProductionSet(LHS: LHS, RHSList: List[RHS]) extends Semgus
case class LHS(NTName: String, NTTerm: String, NTRel: SMTFormula) extends Semgus
case class RHS(RHSExp: RHSExp, Premises: List[SMTFormula]) extends Semgus

sealed trait RHSExp extends Semgus
sealed trait RHSAtom extends RHSExp

case class RHSOp(opName: String, args: List[RHSAtom]) extends RHSExp
case class RHSNT(NTName: String, NTTerm: String) extends RHSAtom
case class RHSLeaf(leafName: String) extends RHSAtom

case class SMTFormula(formula: String) extends Semgus
