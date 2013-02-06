package improving

import improving.fusion.Fusion._

object Demo {
  var dummy = 0
  val Max = 1 << 22
  val xs = 1 to Max toList
  def inc(x: Int) = x + 1

  def f(xs: List[Int]) = xs map inc map inc map inc filter (_ % 2 != 0) filter (_ % 3 != 0) filter (_ % 5 != 0)
  def g(xs: List[Int]) = xs map inc map inc map inc filter (_ % 2 != 0) filter (_ % 3 != 0) filter (_ % 5 != 0) fuse

  def timed(what: String, num: Int, fn: List[Int] => List[Int]): Unit = {
    val start  = System.nanoTime
    val result = fn(xs take num).sum
    val end    = System.nanoTime

    dummy += result
    println("n=%8s  %2s  %.3f ms".format(num, what, (end - start).toDouble / 1e6))
  }

  def main(args: Array[String]): Unit = {
    var i = 1
    while (i <= Max) {
      timed("f", i, f _)
      timed("g", i, g _)
      i *= 2
    }
  }
}

/**
    -Xprint:typer

    def f: List[Int] = immutable.this.List.apply[Int](1, 2, 3).map[Int, List[Int]]({
  ((x: Int) => Test.this.inc(x))
})(immutable.this.List.canBuildFrom[Int]).map[Int, List[Int]]({
  ((x: Int) => Test.this.inc(x))
})(immutable.this.List.canBuildFrom[Int]).map[Int, List[Int]]({
      ((x: Int) => Test.this.inc(x))
    })(immutable.this.List.canBuildFrom[Int]);

    def g: List[Int] = FusionOps.apply[List[Int]](immutable.this.List.apply[Int](1, 2, 3).map[Int, List[Int]]({
  ((x: Int) => Test.this.inc(x))
}.andThen[Int]({
  ((x: Int) => Test.this.inc(x))
}).andThen[Int]({
  ((x: Int) => Test.this.inc(x))
}))(immutable.this.List.canBuildFrom[Int])).fuse;

**/
