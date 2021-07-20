package genConstraints

import lang._
import lang.Semgus._
import lang.SMT._

object genBasic {
  private var counter = 0
  private val realizableDecl = SMTRelDeclaration("realizable", Nil)
  private val realizableRel = SMTFormulaHolder("realizable")
  private val query = CHCQuery(realizableRel)

  private type NTCtxt = Map[String, Sort]
  private type LeafCtxt = Map[String, Sort]
  private type DeclCtxt = Map[Sort, Set[SMTConstructor]]
  private def genNewAccessor(opName: String): String = {counter = counter + 1; opName + s"Ac$counter"}

  private def genRHSTerm(rhs: RHS): SMTFormula = {
    rhs.rhsExp match {
      case RHSOp(opName, args) => val argString = args.map{
        case RHSNT(_, ntTerm) => ntTerm
        case RHSLeaf(leafName) => leafName
      }.mkString(" ")
        SMTFormula(s"($opName $argString)")
      case RHSNT(ntName, ntTerm) => SMTFormula(s"($ntName $ntTerm)")
      case RHSLeaf(leafName) => SMTFormula(leafName)
    }}

  private def addSyntacticConstraints(prods: List[ProductionSet]): List[ProductionSet] = prods.map{prod =>
    val lhsTerm = prod.lhs.ntTerm
    ProductionSet(prod.lhs, prod.rhsList.map{rhs => RHS(rhs.rhsExp, rhs.premises.map{p =>
      val rhsTerm = genRHSTerm(rhs)
      val synConst = s"(= $lhsTerm $rhsTerm)"
      SMTFormula(s"(and $synConst $p)")
    })
    })
  }

  def genNTCtxt(ntDecls: List[NTDeclaration]): NTCtxt = ntDecls.foldLeft(Map(): NTCtxt){
    (ctxt, ntDecl) => ctxt + (ntDecl.ntName -> ntDecl.ntType)
  }
  def genLeafCtxt(ntCtxt: NTCtxt)(prods: List[ProductionSet]): LeafCtxt = prods.foldLeft(Map(): LeafCtxt){
    (ctxt, prod) => prod.rhsList.foldLeft(ctxt){(rctxt, rhs) => rhs.rhsExp match{
      case _: RHSOp => rctxt
      case _: RHSNT => rctxt
      case RHSLeaf(leafName) => if (rctxt contains leafName) rctxt else rctxt + (leafName -> ntCtxt(prod.lhs.ntName))
    }}}

  def genConstructorFromRHSExp(ntCtxt: NTCtxt)(leafCtxt: LeafCtxt)(rhsExp: RHSExp): SMTConstructor = rhsExp match {
    case RHSOp(opName, args) =>
      val accessors = args.map{
        case RHSNT(ntName, _) => val accessorName = genNewAccessor(opName)
          SMTAccessor(accessorName, ntCtxt(ntName))
        case RHSLeaf(leafName) => val accessorName = genNewAccessor(opName)
          SMTAccessor(accessorName, leafCtxt(leafName))
      }
      SMTOpConstructor(opName, accessors)
    case RHSNT(ntName, _) => val opName = s"single-NT$ntName"
      val accessorName = genNewAccessor(opName)
      SMTOpConstructor(opName, SMTAccessor(accessorName, ntCtxt(ntName))::Nil)
    case RHSLeaf(leafName) => SMTLeafConstructor(leafName)
  }

  def genDatatypeDecl(ntCtxt: NTCtxt)(leafCtxt: LeafCtxt)(prods: List[ProductionSet]): SMTRecDatatypeDeclaration = {
    val constructorMap = prods.foldLeft(Map(): DeclCtxt){(constMap, prodSet) =>
      val rhsConsts = prodSet.rhsList.map{r => genConstructorFromRHSExp(ntCtxt)(leafCtxt)(r.rhsExp)}.toSet
      val lhsType = ntCtxt(prodSet.lhs.ntName)
      if (constMap contains lhsType) constMap + (lhsType -> constMap(lhsType).union(rhsConsts)) else
        constMap + (lhsType -> rhsConsts)
    }
    SMTRecDatatypeDeclaration(
      constructorMap.toList.map{case (termType, consts) => SMTDatatypeDeclaration(termType, consts.toList)})
  }

  def genCHCs(prods: List[ProductionSet]): List[CHCRule] =
    prods.flatMap{prod => prod.rhsList.flatMap{rhs => rhs.premises.map{premise =>
      CHCRule(SMTFormulaHolder(premise.formula), SMTFormulaHolder(prod.lhs.ntRel.formula))
    }}}

  def genVarDecl(varDecl: VarDeclaration): SMTVarDeclaration =
    SMTVarDeclaration(varDecl.varName, varDecl.sortName)
  def genRelDecl(relDecl: RelDeclaration): SMTRelDeclaration =
    SMTRelDeclaration(relDecl.relName, relDecl.args)

  def genSpecificationCHC(constraints: List[Constraint]): CHCRule = {
    val constFormulas = constraints.map{_.formula.formula}
    val formulaStr = constFormulas.mkString(" ")
    val premiseStr = if (constFormulas.length == 1) formulaStr else s"(and $formulaStr)"
    CHCRule(SMTFormulaHolder(premiseStr), realizableRel)
  }

  def semgus2SMT(semgusFile: SemgusFile): List[SMTCommand] = {
    val ntDecls = semgusFile.commands.collect{
      case n: NTDeclaration => n::Nil
      case s: SynthBlock => s.nts
    }.flatten
    val prods = addSyntacticConstraints(semgusFile.commands.collect{case s: SynthBlock => s.prods}.flatten)
    val ntCtxt = genNTCtxt(ntDecls)
    val leafCtxt = genLeafCtxt(ntCtxt)(prods)

    val funName = semgusFile.commands.collect{case s: SynthBlock => SMTVarDeclaration(s.name, s.termType)}
    val varDecls = funName:::semgusFile.commands.collect{
      case v: VarDeclaration => v::Nil
      case s: SynthBlock => s.vars
    }.flatten.map{genVarDecl}
    val relDecls = realizableDecl::semgusFile.commands.collect{
      case r: RelDeclaration => r::Nil
      case s: SynthBlock => s.nts.map{_.ntRel}
    }.flatten.map{genRelDecl}
    val datatypeDecls = genDatatypeDecl(ntCtxt)(leafCtxt)(prods)

    val constraints = semgusFile.commands.collect{
      case c: Constraint => c
    }

    val CHCs = genCHCs(prods)
    val spec = genSpecificationCHC(constraints)

    datatypeDecls::varDecls:::relDecls:::CHCs:::(spec::query::Nil)
  }
}
