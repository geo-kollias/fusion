package improving
package fusionbug

import scala.reflect.makro.Context
import language.experimental.macros
import scala.collection.{ mutable, immutable, generic }
import generic.CanBuildFrom

class FuseContext[T <: Context](val c: T) {
  import c.universe._
  
  val CBFType       = typeOf[CanBuildFrom[_,_,_]]
  val Function1Type = typeOf[Function1[_,_]]
  val MapName       = newTermName("map")

  case class Call(name: TermName, targs: List[Type], argss: List[List[Tree]]) { }

  object PolyCall {
    def unapply(x: Tree): Option[(Tree, Call)] = x match {
      case Apply(fn, args)              => unapply(fn) collect { case (recv, call) => (recv, call.copy(argss = call.argss :+ args)) }
      case TypeApply(fn, targs)         => unapply(fn) collect { case (recv, call) => (recv, call.copy(targs = targs map (_.tpe))) }
      case Select(qual, name: TermName) => Some((qual, Call(name, Nil, Nil)))
      case _                            => None
    }
  }
  
  // show(x)
  object TypedMapOp {
    def unapply(x: Tree): Option[(Tree, Call)] = x match {
      case PolyCall(recv, call @ Call(MapName, el :: col :: Nil, List(arg :: Nil, cbf :: Nil))) =>
        Some((recv, call)) filter (_ => (arg.tpe <:< Function1Type) && (cbf.tpe <:< CBFType))
      case _ =>
        None
    }
  }
  object MapOp {
    // def apply(fn: Tree, args
    def unapply(x: Tree): Option[(Tree, Tree)] = x match {
      case Apply(Select(fn, MapName), arg :: Nil)                              => Some((fn, arg))
      case Apply(TypeApply(Select(fn, MapName), el :: col :: Nil), arg :: Nil) => Some((fn, arg))
      case Apply(fn, arg :: Nil) if arg.tpe <:< CBFType                         => unapply(fn)
      case Apply(Select(fn, _), arg :: Nil)                                    => None
      case Apply(Apply(Select(fn, MapName), arg :: Nil), cbf :: Nil)           => Some((fn, arg))
      case _                                                                   => showRaw(x) ; None
    }
  }
  def dump(t: Tree) {
    t foreach (t0 => System.out.println("%-20s %s".format(t0.getClass.getName split '.' last, t0)))
  }
  
  def fuse(t: Tree): Tree = t

  def makeFusionOps[T: c.TypeTag](incoming: c.Expr[T]): c.Expr[Op[T]] = {
    val fused  = c.Expr[T](fuse(incoming.tree))
    // val string = c.Expr[String](
    val string = c.Expr[String](Literal(Constant("Incoming: %s\n   Fused: %s".format(show(incoming.tree), show(fused.tree)))))

    c.reify(Op(incoming.splice, string.splice))
  }
  
  def mapOps(t0: Tree): (Tree, List[Call]) = {
    def loop(t: Tree): (Tree, List[Call]) = t match {
      case TypedMapOp(t1, call) => loop(t1) match { case (t, cs) => (t, cs :+ call) }
      case _                    => ((t, Nil))
    }
    loop(t0)
  }
}

class Op[T](val x: T, override val toString: String) { def fuse: T = x }

// class FusionOps[T](val incoming: T, val fuse: T, override val toString: String) { }

object Op {
  def apply[T](x: T, str: String) = new Op[T](x, str)
}

object Fusion {
  
  implicit def method[T >: Null](incoming: T): Op[T] = macro method_impl[T]
  def method_impl[T >: Null : c.TypeTag](c: Context)(incoming: c.Expr[T]): c.Expr[Op[T]] = {
    import c.universe._
    
    val ctx = new FuseContext[c.type](c)
    // import ctx._
    
    // val (t1, ops) = mapOps(incoming.tree)
    // println(s"Found ${ops.size} map operations")

    // ctx.c.reify(FusionOps(incoming.splice, null, null))
    // makeFusionOps(incoming)
    // val t = Literal(Constant(null))
    // t setType typeOf[Op[T]]
    // c.Expr[FusionOps[T]](
    ctx.makeFusionOps(incoming)// c.reify(Op(incoming.splice))
  }
}
