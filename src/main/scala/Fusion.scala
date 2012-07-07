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
    private def targs_str = if (targs.isEmpty) "" else targs.mkString("[", ",", "]")
    private def argss_str = argss map (_ mkString ("(", ",", ")")) mkString ""
    override def toString = s"$name$targs_str$argss_str"
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
      case MonoCall(recv, call @ Call(Filter | WithFilter, Nil, (arg :: Nil) :: Nil)) =>
        Some((recv, call)) filter (_ => (arg.tpe <:< Function1Type))
      case _ =>
        None
    }
  }
  // object MapOp {
  //   // def apply(fn: Tree, args
  //   def unapply(x: Tree): Option[(Tree, Tree)] = x match {
  //     case Apply(Select(fn, FusionName(_)), arg :: Nil)                              => Some((fn, arg))
  //     case Apply(TypeApply(Select(fn, FusionName(_)), el :: col :: Nil), arg :: Nil) => Some((fn, arg))
  //     case Apply(fn, arg :: Nil) if arg.tpe <:< CBFType                              => unapply(fn)
  //     case Apply(Select(fn, _), arg :: Nil)                                          => None
  //     case Apply(Apply(Select(fn, FusionName(_)), arg :: Nil), cbf :: Nil)           => Some((fn, arg))
  //     case _                                                                         => showRaw(x) ; None
  //   }
  // }
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
    val (recv, ops) = findFusionOps(incoming.tree)
    val string      = c.Expr[String](Literal(Constant(recv :: ops mkString "\n  .")))

    c.reify(FusionOps(incoming.splice, string.splice))
  }
  
  def findFusionOps(t0: Tree): (Tree, List[Call]) = {
    def loop(t: Tree): (Tree, List[Call]) = t match {
      case FusionCall(t1, call) => loop(t1) match { case (t, cs) => (t, cs :+ call) }
      case _                    => ((t, Nil))
    }
    loop(t0)
  }
}

class FusionOps[T](val incoming: T, override val toString: String) {
  def fuse: T = incoming
  def fuse_show: Unit = println(toString)
}

object FusionOps {
  def apply[T](incoming: T, string: String): FusionOps[T] =
    new FusionOps[T](incoming, string)
}

object Fusion {
  implicit def addFusionOps[T >: Null](incoming: T): FusionOps[T] = macro addFusionOpsImpl[T]
  def addFusionOpsImpl[T >: Null : c.TypeTag](c: Context)(incoming: c.Expr[T]): c.Expr[FusionOps[T]] =
    new FuseContext[c.type](c) makeFusionOps incoming
}
