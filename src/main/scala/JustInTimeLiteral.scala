package improving

import scala.reflect.macros.Context
import scala.language.experimental.macros

final class ShellExpandedString(val sh: String) extends AnyVal

class ShellExpansion[T <: Context](val c: T) {
  def expand(string: c.Expr[String]): c.Expr[ShellExpandedString] = {
    import c.universe._
    import scala.sys.process._

    def expand(s: String) = {
      // val tokens = command.trim split "\\s+" toSeq;
      val expanded = Process(s).lines.mkString("\n")
      // println("expanded = " + expanded)
      expanded
      // Literal(Constant(expanded)) setType definitions.StringClass.typeSignature
    }

    object Expander extends Transformer {
      override def transform(t: Tree): Tree = t match {
        case Literal(Constant(s: String)) => c.typeCheck(Literal(Constant(expand(s))))
        case _                            => super.transform(t)
      }
    }

    val expr = c.Expr[String](Expander transform string.tree)
    reify(new ShellExpandedString(expr.splice))

    // val command: String = string.tree match {
    //   case Literal(Constant(s: String)) => s
    //   case _                            => c.error(c.enclosingPosition, "not a string literal: " + string) ; ""
    // }
    // val tokens = command.trim split "\\s+" toSeq;
    // val expanded = Process(tokens).lines.mkString("\n")
    // println("expanded = " + expanded)
    // val expr = c.Expr[String](Literal(Constant(expanded)))

    // c.reify(new ShellExpandedString(expr.splice))
  }
}

object jitl {
  implicit def shellExpansion(string: String): ShellExpandedString = macro shellExpansionImpl
  def shellExpansionImpl(c: Context)(string: c.Expr[String]): c.Expr[ShellExpandedString] =
    new ShellExpansion[c.type](c) expand string
}
