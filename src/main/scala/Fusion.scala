package improving
package fusion

import scala.reflect.makro.Context
import language.experimental.macros
import scala.collection.{ mutable, immutable, generic }
import generic.CanBuildFrom

class FuseContext[T <: Context](val c: T) {
  import c.universe._
  
  val CBFType       = typeOf[CanBuildFrom[_,_,_]]
  val Function1Type = typeOf[Function1[_,_]]
  
  object Names {
    val Map        = newTermName("map")
    val Filter     = newTermName("filter")
    val WithFilter = newTermName("withFilter")
    val FlatMap    = newTermName("flatMap")
  }
  object FusionName {
    import Names._
    def unapply(x: TermName): Option[TermName] = x match {
      case Map | Filter | WithFilter | FlatMap => Some(x)
      case _                                   => None
    }
  }

  case class Call(name: TermName, targs: List[Type], argss: List[List[Tree]]) {
    def applyTo(recv: Tree) = {
      val t1 = Select(recv, name)
      val t2 = if (targs.isEmpty) t1 else TypeApply(t1, targs map TypeTree)
      val t3 = argss.foldLeft(t2)((fn, args) => Apply(fn, args))
     
      t3
    }
    private def targs_str = if (targs.isEmpty) "" else targs.mkString("[", ",", "]")
    private def argss_str = argss map (_ mkString ("(", ",", ")")) mkString ""
    override def toString = s"$name$targs_str$argss_str"
  }
  
  import Names.{ Map, Filter }

  def fuseCalls(calls: List[Call]): List[Call] = calls match {
    case Nil | List(_) =>
      calls

    case Call(Filter, Nil, List(f1) :: Nil) :: Call(Filter, Nil, List(f2) :: Nil) :: rest =>
      // val and   = Apply(Select(f1, newTermName("&&")), List(f2))
      val f1expr = c.Expr(f1)
      val f2expr = c.Expr(f2)
      val and = c.reify(improving.fusion.Fusion.and(f1expr.splice, f2expr.splice))
      val fused = Call(Filter, Nil, List(and.tree) :: Nil)
      fuseCalls(fused :: rest)
      
    case Call(Map, _, List(f1) :: _) :: Call(Map, _, List(f2) :: List(cbf) :: _) :: rest =>
      val andThen = Apply(Select(f1, newTermName("andThen")), List(f2))
      val fused   = Call(Map, Nil, List(andThen) :: List(cbf) :: Nil)
      fuseCalls(fused :: rest)

    case x :: rest => 
      x :: fuseCalls(rest)
  }

  object PolyCall {
    def unapply(x: Tree): Option[(Tree, Call)] = x match {
      case Apply(fn, args)              => unapply(fn) collect { case (recv, call) => (recv, call.copy(argss = call.argss :+ args)) }
      case TypeApply(fn, targs)         => unapply(fn) collect { case (recv, call) => (recv, call.copy(targs = targs map (_.tpe))) }
      case Select(qual, name: TermName) => Some((qual, Call(name, Nil, Nil)))
      case _                            => None
    }
  }
  object MonoCall {
    def unapply(x: Tree): Option[(Tree, Call)] = x match {
      case Apply(fn, args)              => unapply(fn) collect { case (recv, call) => (recv, call.copy(argss = call.argss :+ args)) }
      case Select(qual, name: TermName) => Some((qual, Call(name, Nil, Nil)))
      case _                            => None
    }
  }

  // show(x)
  object FusionCall {
    import Names._

    def unapply(x: Tree): Option[(Tree, Call)] = x match {
      case PolyCall(recv, call @ Call(Map | FlatMap, el :: col :: Nil, List(arg :: Nil, cbf :: Nil))) =>
        Some((recv, call)) filter (_ => (arg.tpe <:< Function1Type) && (cbf.tpe <:< CBFType))
      case MonoCall(recv, call @ Call(Filter | WithFilter, Nil, List(arg :: Nil))) =>
        Some((recv, call)) filter (_ => (arg.tpe <:< Function1Type))
      case _ =>
        None
    }
  }

  def dump(t: Tree) {
    t foreach (t0 => System.out.println("%-20s %s".format(t0.getClass.getName split '.' last, t0)))
  }
  
  def fuse(t: Tree): Tree = {
    val t1 = t.duplicate
    c.resetAllAttrs(t)
    c.resetAllAttrs(t1)
    t1
  }

  def makeFusionOps[T: c.TypeTag](incoming: c.Expr[T]): c.Expr[FusionOps[T]] = {
    val (recv, ops0) = findFusionOps(incoming.tree)
    val ops          = fuseCalls(ops0)
    val s1           = recv :: ops0 mkString "\n  ."
    val s2           = recv :: ops mkString "\n  ."
    val string       = c.Expr[String](Literal(Constant(s1 + "\n\n" + s2)))
    
    def fused = c.Expr[T](ops.foldLeft(recv)((t, op) => op applyTo t))

    c.reify(FusionOps(fused.splice))  //, string.splice))
  }
  
  def findFusionOps(t0: Tree): (Tree, List[Call]) = {
    def loop(t: Tree): (Tree, List[Call]) = t match {
      case FusionCall(t1, call) => loop(t1) match { case (t, cs) => (t, cs :+ call) }
      case _                    => ((t, Nil))
    }
    loop(t0)
  }
}

@inline final class FusionOps[T](val fuse: T) extends AnyVal {
  // lazy val fuse: T = incoming
  // def fuse_show: Unit = println(toString)
}
// 
// class FusionOps[T](incoming: => T, override val toString: String) {
//   lazy val fuse: T = incoming
//   def fuse_show: Unit = println(toString)
// }

object FusionOps {
  def and[T](f1: T => Boolean, f2: T => Boolean): T => Boolean = (x: T) => f1(x) && f2(x)
  
  def apply[T](incoming: T): FusionOps[T] =
    new FusionOps[T](incoming)//, "")
  // def apply[T](incoming: T, string: String): FusionOps[T] =
  //   new FusionOps[T](incoming, string)
}

object Fusion {
  def and[T](f1: T => Boolean, f2: T => Boolean): T => Boolean = (x: T) => f1(x) && f2(x)
  def or[T](f1: T => Boolean, f2: T => Boolean): T => Boolean = (x: T) => f1(x) || f2(x)
  
  implicit class BooleanFunction1Ops[T](val f1: T => Boolean) extends AnyVal {
    def && (f2: T => Boolean): T => Boolean = (x: T) => f1(x) && f2(x)
    def || (f2: T => Boolean): T => Boolean = (x: T) => f1(x) || f2(x)
  }
  
  implicit def addFusionOps[T >: Null](incoming: T): FusionOps[T] = macro addFusionOpsImpl[T]
  def addFusionOpsImpl[T >: Null : c.TypeTag](c: Context)(incoming: c.Expr[T]): c.Expr[FusionOps[T]] =
    new FuseContext[c.type](c) makeFusionOps incoming
}
