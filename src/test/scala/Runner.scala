package improving
package fusion

import Fusion._
// import fusionbug.Fusion._

object Runner {
  // val xs = 1.fuse
  val zs = List(1, 2, 3) map (_ + 1) map (_ - 1) fuse
  
  // scala.reflect.runtime.universe reify { List(1) map (_ + 1) }
}
