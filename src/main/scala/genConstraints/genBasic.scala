package genConstraints

import lang._
import lang.Semgus._
import lang.SMT._

object genBasic {
  private var counter = 0
  private type NTCtxt = Map[String, Sort]
  private type LeafCtxt = Map[String, Sort]
  private type DeclCtxt = Map[Sort, List[SMTConstructor]]
  private def genNewAccessor(opName: String): String = opName + s"Ac$counter"; counter += 1

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
      val rhsConsts = prodSet.rhsList.map{r => genConstructorFromRHSExp(ntCtxt)(leafCtxt)(r.rhsExp)}
      val lhsType = ntCtxt(prodSet.lhs.ntName)
      if (constMap contains lhsType) constMap + (lhsType -> (constMap(lhsType):::rhsConsts)) else
        constMap + (lhsType -> rhsConsts)
    }
    SMTRecDatatypeDeclaration(
      constructorMap.toList.map{case (termType, consts) => SMTDatatypeDeclaration(termType, consts)})
  }

  def genVarDecl(varDecl: VarDeclaration): SMTVarDeclaration = SMTVarDeclaration(varDecl.varName, varDecl.sortName)
  def genRelDecl(relDecl: RelDeclaration): SMTRelDeclaration = SMTRelDeclaration(relDecl.relName, relDecl.args)

  def semgus2SMT(semgusFile: SemgusFile): List[SMTCommand] = {
    val ntDecls = semgusFile.commands.collect{
      case n: NTDeclaration => n::Nil
      case s: SynthBlock => s.nts
    }.flatten
    val prods = semgusFile.commands.collect{
      case s: SynthBlock => s.prods
    }.flatten
    val ntCtxt = genNTCtxt(ntDecls)
    print(ntCtxt)
    val leafCtxt = genLeafCtxt(ntCtxt)(prods)

    val varDecls = semgusFile.commands.collect{
      case v: VarDeclaration => v::Nil
      case s: SynthBlock => s.vars
    }.flatten.map{genVarDecl}
    val relDecls = semgusFile.commands.collect{
      case r: RelDeclaration => r::Nil
      case s: SynthBlock => s.nts.map{_.ntRel}
    }.flatten.map{genRelDecl}
    val datatypeDecls = genDatatypeDecl(ntCtxt)(leafCtxt)(prods)

    datatypeDecls::varDecls:::relDecls
  }
}
