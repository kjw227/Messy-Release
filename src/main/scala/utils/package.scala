package object utils {

  def filterNones[A](l: List[Option[A]]): List[A] = l.filter{case None => false; case Some(_) => true}.map{_.get}

  def printMagenta(s: String): Unit = println(Console.MAGENTA + s + Console.RESET)
  def printCyan(s: String): Unit = println(Console.CYAN + s + Console.RESET)
  def printYellow(s: String): Unit = println(Console.YELLOW + s + Console.RESET)
  def printGreen(s: String): Unit = println(Console.GREEN + s + Console.RESET)
  def printRed(s: String): Unit = println(Console.RED + s + Console.RESET)
}
