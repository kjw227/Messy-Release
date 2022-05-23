package genConstraints

import lang._
import lang.Semgus._
import lang.SMT._

case class translateSMTException(message: String) extends Exception(message)

object genBasic {
  private var counter = 0
  private val realizableDecl = SMTRelDeclaration("realizable", Nil)
  private val realizableRel = SMTFormulaHolder("realizable")
  private val query = CHCQuery(realizableRel)

  private type NTCtxt = Map[String, Sort]
  private type LeafCtxt = Map[String, Sort]
  private type DeclCtxt = Map[Sort, Set[SMTConstructor]]
  private def genNewAccessor(opName: String): String = {counter = counter + 1; opName + s"Ac$counter"}
  private def synRelName(ntName: String): String = s"$ntName.Syn"
  private def synVarName(ntName: String)(index: Int): String = s"$ntName.SynVar$index"

  /*
  private def genRHSTerm(rhs: RHS): SMTFormula = {
    rhs.rhsExp match {
      case RHSOp(opName, args) => val argString = args.map{
        case RHSNT(Nonterminal(ntName, ntType)) => ntName
        case RHSLeaf(leafName) => leafName
      }.mkString(" ")
        SMTFormula(s"($opName $argString)")
      case RHSNT(Nonterminal(ntName, ntType)) => SMTFormula(s"($ntName $ntType)")
      case RHSLeaf(leafName) => SMTFormula(leafName)
    }}
   */

  /*
  private def addSyntacticConstraints(prods: List[LHSProductionSet]): List[LHSProductionSet] = prods.map{ prod =>
    val lhsTerm = prod.lhs.ntTerm
    LHSProductionSet(prod.lhs, prod.rhsList.map{ rhs => RHS(rhs.rhsExp, rhs.premises.map{ p =>
      val rhsTerm = genRHSTerm(rhs)
      val synConst = s"(= $lhsTerm $rhsTerm)"
      SMTFormula(s"(and $synConst $p)")
    })
    })
  }
  */

  def genNTCtxt(prods: List[LHSProductionSet]): NTCtxt = prods.foldLeft(Map(): NTCtxt){
    (ctxt, ntDecl) => ctxt + (ntDecl.lhs.nt.ntName -> ntDecl.lhs.nt.ntType)
  }

  def genLeafCtxt(ntCtxt: NTCtxt)(prods: List[LHSProductionSet]): LeafCtxt = prods.foldLeft(Map(): LeafCtxt){
    (ctxt, prod) => prod.rhsList.foldLeft(ctxt){(rctxt, rhs) => rhs.rhsExp match{
      case _: RHSOp => rctxt
      case _: RHSNT => rctxt
      case RHSLeaf(leafName) => if (rctxt contains leafName) rctxt else rctxt + (leafName -> ntCtxt(prod.lhs.nt.ntName))
    }}}

  def genConstructorFromRHSExp(ntCtxt: NTCtxt)(leafCtxt: LeafCtxt)(rhsExp: RHSExp): SMTConstructor = rhsExp match {
    case RHSOp(opName, args) =>
      val accessors = args.map{
        case RHSNT(nt) => val accessorName = genNewAccessor(opName)
          SMTAccessor(accessorName, ntCtxt(nt.ntName))
        case RHSLeaf(leafName) => val accessorName = genNewAccessor(opName)
          SMTAccessor(accessorName, leafCtxt(leafName))
      }
      SMTOpConstructor(opName, accessors)
    case RHSNT(nt) => val ntName = nt.ntName; val opName = s"single-NT$ntName"
      val accessorName = genNewAccessor(opName)
      SMTOpConstructor(opName, SMTAccessor(accessorName, ntCtxt(ntName))::Nil)
    case RHSLeaf(leafName) => SMTLeafConstructor(leafName)
  }

  def genDatatypeDecl(ntCtxt: NTCtxt)(leafCtxt: LeafCtxt)(prods: List[LHSProductionSet]): SMTRecDatatypeDeclaration = {
    val constructorMap = prods.foldLeft(Map(): DeclCtxt){(constMap, prodSet) =>
      val rhsConsts = prodSet.rhsList.map{r => genConstructorFromRHSExp(ntCtxt)(leafCtxt)(r.rhsExp)}.toSet
      val lhsType = ntCtxt(prodSet.lhs.nt.ntName)
      if (constMap contains lhsType) constMap + (lhsType -> constMap(lhsType).union(rhsConsts)) else
        constMap + (lhsType -> rhsConsts)
    }
    SMTRecDatatypeDeclaration(
      constructorMap.toList.map{case (termType, consts) => SMTDatatypeDeclaration(termType, consts.toList)})
  }

  def genSemRelDecls(chc: SemanticCHC): SMTRelDeclaration = genRelDecl(chc.decl)
  def genSemVarDecls(chc: SemanticCHC): Set[SMTVarDeclaration] =
    chc.vars.foldLeft(Set(): Set[SMTVarDeclaration]){case (acc, v) => acc + SMTVarDeclaration(v.name, v.tp)}
  def translateCHC(chc: SemanticCHC): CHCRule =
    CHCRule(SMTFormulaHolder(chc.head.formula), SMTFormulaHolder(chc.tail.formula))

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

  def translateLHSProductionSet(l: List[LHSProductionSet]): (NTCtxt, LeafCtxt, SMTRecDatatypeDeclaration) = {
    val ntCtxt = genNTCtxt(l)
    val leafCtxt = genLeafCtxt(ntCtxt)(l)
    val datatypeDecl = genDatatypeDecl(ntCtxt)(leafCtxt)(l)
    (ntCtxt, leafCtxt, datatypeDecl)
  }

  def genSyntaxDecls(l: List[LHSProductionSet]): Set[SMTRelDeclaration] = l.map{
    case LHSProductionSet(lhs, _) => SMTRelDeclaration(synRelName(lhs.nt.ntName), lhs.nt.ntType::Nil)
  }.toSet

  def genSyntaxRules(l: List[LHSProductionSet]): (List[CHCRule], Set[SMTVarDeclaration]) = {
    val chcDecls = l.map{
      case LHSProductionSet(lhs, rhslist) =>
        val headVar = synVarName(lhs.nt.ntName)(0)
        val headVarDecl = SMTVarDeclaration(headVar, lhs.nt.ntType)
        val headRel = synRelName(lhs.nt.ntName)
        val head = SMTFormulaHolder(s"($headRel $headVar)")
        val premiseDecls = rhslist.map{
          case RHS(RHSOp(opName, args)) =>
            val rhsTermString = args.zipWithIndex.map{
              case (RHSNT(nt), i) => synVarName(nt.ntName)(i + 1)
              case (RHSLeaf(leafName), _) => leafName
            }.mkString(" ")
            val eqString = s"(= $headVar ($opName $rhsTermString))"
            val rhsPremises = args.zipWithIndex.map{
              case (RHSNT(nt), i) =>
                val rhsVar = synVarName(nt.ntName)(i + 1)
                val rhsRel = synRelName(nt.ntName)
                SMTFormulaHolder(s"($rhsRel $rhsVar)")
              case (RHSLeaf(_), _) => SMTFormulaHolder(s"true")
            }.mkString(" ")

            val rhsVarDecls = args.zipWithIndex.foldLeft(Set(): Set[SMTVarDeclaration]){
              case (acc, (RHSNT(nt), i)) => acc + SMTVarDeclaration(synVarName(nt.ntName)(i + 1), nt.ntType)
              case (acc, (RHSLeaf(_), _)) => acc
            }
            (SMTFormulaHolder(s"(and $eqString $rhsPremises)"), rhsVarDecls)

          case RHS(RHSNT(rhsnt)) =>
            val rhsVar = synVarName(rhsnt.ntName)(1)
            val rhsRel = synRelName(rhsnt.ntName)
            (SMTFormulaHolder(s"and ((= $rhsVar $headVar) ($rhsRel $rhsVar))"),
              Set(SMTVarDeclaration(synVarName(rhsnt.ntName)(1), rhsnt.ntType)))

          case RHS(RHSLeaf(leafName)) =>
            (SMTFormulaHolder(s"(= $headVar $leafName)"), Set(): Set[SMTVarDeclaration])
        }
        val premises = premiseDecls.map{_._1}
        val decls = premiseDecls.map{_._2}.foldLeft(Set(): Set[SMTVarDeclaration]){case (acc, x) => acc.union(x)} +
          headVarDecl
        (premises.map{p => CHCRule(p, head)}, decls)
    }
    val chcs = chcDecls.flatMap{_._1}
    val vardecs = chcDecls.foldLeft(Set(): Set[SMTVarDeclaration]){case (acc, x) => acc.union(x._2)}
    (chcs, vardecs)
  }

  def translateSynthFun(s: SynthFun): (Set[SMTVarDeclaration], Set[SMTRelDeclaration], List[CHCRule]) = {
    val (syntaxRules, syntaxVarDecls) = genSyntaxRules(s.grm)
    val syntaxRelDecls = genSyntaxDecls(s.grm)
    val funDecl = SMTVarDeclaration(s.name, s.termType)
    (syntaxVarDecls + funDecl, syntaxRelDecls, syntaxRules)
  }

  def semgus2SMT(semgusFile: SemgusFile): List[SMTCommand] = {
    val univGrm = utils.filterNones(semgusFile.commands.map{case l: LHSProductionSet => Some(l); case _ => None})
    val constraints = utils.filterNones(semgusFile.commands.map{case c: Constraint => Some(c); case _ => None})
    val synthFuns = utils.filterNones(semgusFile.commands.map{case s: SynthFun => Some(s); case _ => None})
    val chcEvents = utils.filterNones(semgusFile.commands.map{case c: SemanticCHC => Some(c); case _ => None})
    val rest = semgusFile.commands.filter{
      case _: Constraint => false; case _: SynthFun => false; case _: LHSProductionSet => false; case _ => true}

    val (nctxt, lctxt, datatypeDecls) = translateLHSProductionSet(univGrm)
    val semanticRules = chcEvents.map{translateCHC}
    val semanticVarDecls = chcEvents.foldLeft(Set(): Set[SMTVarDeclaration]){
      case (acc, event) => acc.union(genSemVarDecls(event))}.toList
    val semanticDecls = chcEvents.foldLeft(Set(): Set[SMTRelDeclaration]){
      case (acc, event) => acc + genSemRelDecls(event)}.toList

    val (syntaxVarDecls, syntaxRelDecls, syntaxRules) =
      synthFuns.foldLeft((Set(): Set[SMTVarDeclaration], Set(): Set[SMTRelDeclaration], Nil: List[CHCRule])){
        case ((varacc, relacc, ruleacc), s) => val (varset, relset, rulelist) = translateSynthFun(s)
        (varacc.union(varset), relacc.union(relset), ruleacc:::rulelist)
    }

    val specCHC = genSpecificationCHC(constraints)
    datatypeDecls::semanticVarDecls:::syntaxVarDecls.toList:::semanticDecls:::(realizableDecl::Nil):::
      syntaxRelDecls.toList:::syntaxRules:::semanticRules:::(specCHC::query::Nil)
  }
}
