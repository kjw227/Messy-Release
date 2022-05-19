package parse

import lang.SExpr._
import lang.Semgus._

class TranslatorException(message: String) extends Exception(message)

object Translator {
  def parseNewSortDeclaration(): SortDeclaration = ???

  def parseNewVarDeclaration(varNameArgs: SExpr, sortNameArg: SExpr): List[VarDeclaration] = {
    val varNames = varNameArgs match {
      case SExprName(s) => s::Nil
      case SExprList(slist) =>
        slist.map{case SExprName(s) => s; case _ => throw new TranslatorException("Malformed var names in declare-var")}
      case _ => throw new TranslatorException("Predefined names not allowed as variable names")
    }
    val sortName = sortNameArg match {
      case SExprName(s) => s
      case _ => throw new TranslatorException("Malformed sort name in declare-var")
    }
    varNames.map{VarDeclaration(_, sortName)}
  }

  def parseNewRelDeclaration(args: List[SExpr]): RelDeclaration = {
    val relName = args.head match {
      case SExprName(name) => name
      case _ => throw new TranslatorException("Malformed relation name in declare-rel")
    }
    val argSorts = args.tail.head match {
      case SExprList(slist) => slist.map {
        case SExprName(name) => name
        case _ => throw new TranslatorException("Malformed sort argument in declare-rel")
      }
      case _ => throw new TranslatorException("Malformed sort block in declare-rel")
    }
    RelDeclaration(relName, argSorts)
  }

  def parseNewNTDeclaration(ntNameArg: SExpr, ntTypeArg: SExpr, ntRelArg: SExpr): NTDeclaration = {
    val ntName = ntNameArg match {
      case SExprName(s) => s
      case _ => throw new TranslatorException("Malformed nonterminal name in declare-nt")
    }
    val ntType = ntTypeArg match {
      case SExprName(s) => s
      case _ => throw new TranslatorException("Malformed term-type name in declare-nt")
    }
    val ntRel = ntRelArg match {
      case SExprList(slist) => parseNewRelDeclaration(slist)
      case _ => throw new TranslatorException("Malformed relation declaration in declare-nt")
    }
    NTDeclaration(ntName, ntType, ntRel)
  }

  def parseNewSynthBlock(termNameArg: SExpr, termTypeArg: SExpr, grmBlockArg: SExpr): SynthFun = {
    val name = termNameArg match {
      case SExprName(s) => s
      case _ => throw new TranslatorException("Malformed synth-term name in synth-term")
    }
    val termTp = termTypeArg match {
      case SExprName(s) => s
      case _ => throw new TranslatorException("Malformed synth-term term-type in synth-term")
    }
    val cmds = grmBlockArg match {
      case SExprList(slist) => slist.flatMap{parseSExprCommand}
      case _ => throw new TranslatorException("Grammar block malformed in synth-term")
    }
    val nts = cmds.collect{case x: NTDeclaration => x}
    val vars = cmds.collect{case x: VarDeclaration => x}
    val prodSets = cmds.collect{case x: LHSProductionSet => x}

    SynthFun(name, termTp, ???)
  }

  def parseNewProductionSet(lhsNT: SExpr, lhsRel: SExpr, rhsListArg: List[SExpr]): LHSProductionSet = {
    val lhs = parseNewLHS(lhsNT, lhsRel)
    val rhsList = rhsListArg.map{
      case SExprList(rhsExp::rhsPremises) => parseNewRHS(rhsExp, rhsPremises)
      case _ => throw new TranslatorException("Malformed RHS in nonterminal expression")
    }
    LHSProductionSet(lhs, rhsList)
  }

  def parseNewLHS(ntArg: SExpr, relArg: SExpr): LHS = {
    val (lhsName, lhsTerm) = ntArg match {
      case SExprList(SExprName(n)::SExprName(t)::Nil) => (n, t)
      case _ => throw new TranslatorException(s"Malformed LHS nonterminal expression")
    }
    val rel = parseNewSMTFormula(relArg)
    LHS(NonTerminal(lhsName, lhsName))
  }

  def parseNewRHS(expArg: SExpr, premisesArg: List[SExpr]): RHS =
    RHS(parseNewRHSExp(expArg))

  def parseNewRHSExp(expArg: SExpr): RHSExp = expArg match {
      case SExprList(SExprName(ntName)::SExprName(ntTerm)::Nil) => RHSNT(NonTerminal(ntName, ntName))
      case SExprName(leafName) => RHSLeaf(leafName)
      case SExprList(SExprName(opName)::operandList) =>
        val args = operandList.map{
          case SExprList(SExprName(ntName)::SExprName(ntTerm)::Nil) => RHSNT(NonTerminal(ntName, ntName))
          case SExprName(leafName) => RHSLeaf(leafName)
          case _ => throw new TranslatorException("Only nonterminals and leafs are allowed as operands of an RHS")
        }
        RHSOp(opName, args)
      case _ => throw new TranslatorException("Malformed RHS expression")
    }

  def parseNewSMTFormula(fArg: SExpr): SMTFormula = SMTFormula(fArg.toString)

  def parseNewConstraint(fArg: SExpr): Constraint = Constraint(parseNewSMTFormula(fArg))

  def parseSExprCommand(s: SExpr): List[SemgusElement] = s match {
    case _: SExprLeaf => throw new TranslatorException("Encountered S-Expression leaf where command was expected")
    case SExprList(slist) => slist.head match {
      case SExprName("declare-sort") => parseNewSortDeclaration()::Nil

      case SExprName("declare-var") =>
        if (slist.length != 3) throw new TranslatorException("Wrong number of arguments to declare-var")
        else parseNewVarDeclaration(slist(1), slist(2))

      case SExprName("declare-rel") =>
        if (slist.length != 3) throw new TranslatorException("Wrong number of arguments to declare-rel")
        parseNewRelDeclaration(slist.tail)::Nil

      case SExprName("declare-nt") =>
        if (slist.length != 4) throw new TranslatorException("Wrong number of arguments to declare-nt")
        else parseNewNTDeclaration(slist(1), slist(2), slist(3))::Nil

      case SExprName("synth-term") =>
        if (slist.length != 4) throw new TranslatorException("Wrong number of arguments to synth-term")
        else parseNewSynthBlock(slist(1), slist(2), slist(3))::Nil

      case SExprName("constraint") =>
        if (slist.length != 2) throw new TranslatorException("Wrong number of arguments to constraint")
        else parseNewConstraint(slist(1))::Nil

      case SExprList(_::_::Nil) => parseNewProductionSet(slist.head, slist.tail.head, slist.tail.tail)::Nil

      case _ => throw new TranslatorException("Unknown command type in translating S-Expressions")
    }
  }

  def parseSemgusFile(s: SExpr): SemgusFile = s.removeAnnotations match {
    case SExprList(slist) => SemgusFile(slist.flatMap(parseSExprCommand))
    case _ => throw new TranslatorException("Top-level Semgus file must consist of a list of commands")
  }
}
