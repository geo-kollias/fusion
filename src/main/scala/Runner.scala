import Macro._

object Test {
  // val xs = method[Int](1).fuse
  // val xs = List(1,2,3).fuse
  val xs = 1.fuse
  val ys = (List(1, 2, 3) map (_ + 1) map (_ - 1)).fuse
  
  def main(args: Array[String]): Unit = {
  }
}
