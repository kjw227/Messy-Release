package lang
package SMT

sealed trait SMT {
  override def toString: String = this match {
    case Skip => ""
    case SMTVarDeclaration(varName, sortName) => s"(declare-var $varName $sortName)"
    case SMTRelDeclaration(relName, sortNames) => val sortStrs = sortNames.mkString(" ")
      s"(declare-rel $relName ($sortStrs))"
    case SMTDatatypeDeclaration(typeName, constructors) => val constructorStrs = constructors.mkString(" ")
      s"($typeName $constructorStrs)"
    case SMTRecDatatypeDeclaration(types) => val typeStrs = types.mkString("\n")
       s"(declare-datatypes () ($typeStrs))"
    case CHCRule(premise, conclusion) => s"(rule (=> $premise $conclusion))"
    case CHCQuery(query) => s"(query $query)"
    case SMTFormulaHolder(formula) => formula
  }
}

sealed trait SMTCommand extends SMT
sealed trait Declaration extends SMTCommand
sealed trait SMTExpr extends SMTCommand

case object Skip extends SMTCommand
case class SMTVarDeclaration(varName: String, sortName: Sort) extends Declaration
case class SMTRelDeclaration(relName: String, sortNames: List[Sort]) extends Declaration
case class SMTDatatypeDeclaration(typeName: Sort, constructors: List[SMTConstructor]) extends Declaration
case class SMTRecDatatypeDeclaration(types: List[SMTDatatypeDeclaration]) extends Declaration

sealed trait SMTConstructor{
  override def toString: String = this match {
    case SMTOpConstructor(name, accessors) => val accString = accessors.mkString(" "); s"($name $accString)"
    case SMTLeafConstructor(name) => name
  }
}
case class SMTAccessor(accessorName: String, argSort: Sort){
  override def toString: String = s"($accessorName $argSort)"
}
case class SMTOpConstructor(name: String, accessors: List[SMTAccessor]) extends SMTConstructor
case class SMTLeafConstructor(name: String) extends SMTConstructor

case class CHCRule(premise: SMTFormulaHolder, conclusion: SMTFormulaHolder) extends SMTCommand
case class CHCQuery(query: SMTFormulaHolder) extends SMTCommand

case class SMTFormulaHolder(formula: String) extends SMTExpr
