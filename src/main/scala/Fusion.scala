import scala.reflect.makro.Context
import language.experimental.macros
import scala.collection.{ mutable, immutable, generic }
import generic.CanBuildFrom

class MacroOps[T](val x: T, val fuse: T) { }
object MacroOps {
  def apply[T](x: T): MacroOps[T] = new MacroOps[T](x, x)
}

object Macro {
  implicit def method[T](x: T): MacroOps[T] = macro method_impl[T]
  def method_impl[T: c.TypeTag](c: Context)(x: c.Expr[T]): c.Expr[MacroOps[T]] = {
    import c.universe._
    val CbfTpe       = typeOf[CanBuildFrom[_,_,_]]
    val Function1Tpe = typeOf[Function1[_,_]]
    val MapName = newTermName("map")
    
    case class Call(name: TermName, targs: List[Type], argss: List[List[Tree]]) { }

    object PolyCall {
      def unapply(x: Tree): Option[(Tree, Call)] = x match {
        case Apply(fn, args)              => unapply(fn) collect { case (recv, call) => (recv, call.copy(argss = argss :+ args) }
        case TypeApply(fn, targs)         => unapply(fn) collect { case (a, b, Nil, argss) => (a, b, targs map (_.tpe), argss) }
        case Select(qual, name: TermName) => Some((qual, name, Nil, Nil))
        case _                            => None
      }
    }
    
    // show(x)
    object TypedMapOp {
      def unapply(x: Tree): Option[(Type, Type, Tree, Tree)] = x match {
        case Apply(Apply(TypeApply(Select(fn, MapName), el :: col :: Nil), arg :: Nil), cbf :: Nil) =>
          if ((arg.tpe <:< Function1Tpe) && (cbf.tpe <:< CbfTpe))
            Some((el.tpe, col.tpe, fn, arg))
          else None
        case  _ => None
      }
    }
    object MapOp {
      // def apply(fn: Tree, args
      def unapply(x: Tree): Option[(Tree, Tree)] = x match {
        case Apply(Select(fn, MapName), arg :: Nil)                              => Some((fn, arg))
        case Apply(TypeApply(Select(fn, MapName), el :: col :: Nil), arg :: Nil) => Some((fn, arg))
        case Apply(fn, arg :: Nil) if arg.tpe <:< CbfTpe                         => unapply(fn)
        case Apply(Select(fn, _), arg :: Nil)                                    => None
        case Apply(Apply(Select(fn, MapName), arg :: Nil), cbf :: Nil)           => Some((fn, arg))
        case _                                                                   => showRaw(x) ; None
      }
    }
    def dump(t: Tree) {
      t foreach (t0 => System.out.println("%-20s %s".format(t0.getClass.getName split '.' last, t0)))
    }
      
    // 
    // x.tree foreach (t => show(t))
    x.tree match {
      case TypedMapOp(el, coll, closure, cbf) =>
        println("ok cbf!!")
        dump(closure)
        dump(cbf)
        println("" + (el, coll))
        
      case MapOp(fn, arg) => println("ok!") ; dump(fn) ; println("\nand arg:\n\n") ; dump(arg)
      case t              => println("not ok") ; dump(t) ; println(show(t)) ; println(showRaw(t))
    }
      // case MapOp(MapOp(xs, f1), f2) => 
    
    // throw new Exception
    // System.out.println("macro: " + x)
    // c.warning(c.enclosingPosition, "macro: " + x)
    c.reify(MacroOps(x.splice))
  }
}
