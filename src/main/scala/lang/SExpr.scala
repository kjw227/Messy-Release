package lang
package SExpr

sealed trait SExpr {
  override def toString: String = this match {
    case SExprInt(v) => v.toString
    case SExprBool(v) => v.toString
    case SExprString(s) => s"(String: s)"
    case SExprBV(v) => s"(BV: #x" + f"$v%08x" + ")"
    case SExprName(s) => s"(Name: $s)"
    case SExprList(slist) => val listStr = slist.map(_.toString).mkString(" "); s"(list: $listStr)"
  }
}
sealed trait SExprLeaf extends SExpr

case class SExprInt(v: Int) extends SExprLeaf
case class SExprBool(v: Boolean) extends SExprLeaf
case class SExprString(s: String) extends SExprLeaf
case class SExprBV(v: Int) extends SExprLeaf
case class SExprName(s: String) extends SExprLeaf
case class SExprList(slist: List[SExpr]) extends SExpr
