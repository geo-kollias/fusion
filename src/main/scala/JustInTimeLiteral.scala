package improving

import scala.reflect.makro.Context
import language.experimental.macros

final class ShellExpandedString(val sh: String) extends AnyVal

class ShellExpansion[T <: Context](val c: T) {
  def expand(string: c.Expr[String]): c.Expr[ShellExpandedString] = {
    import c.universe._
    import scala.sys.process._

    val command: String = string.tree match {
      case Literal(Constant(s: String)) => s
      case _                            => c.error(c.enclosingPosition, "not a string literal: " + string) ; ""
    }
    val expanded = c.Expr[String](Literal(Constant(Process(command).lines.mkString("\n"))))
    
    c.reify(new ShellExpandedString(expanded.splice))
  }
}

object jitl {
  implicit def shellExpansion(string: String): ShellExpandedString = macro shellExpansionImpl
  def shellExpansionImpl(c: Context)(string: c.Expr[String]): c.Expr[ShellExpandedString] =
    new ShellExpansion[c.type](c) expand string
}
