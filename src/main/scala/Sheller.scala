// package improving
// import jitl._
//
// object Sheller {
//   implicit class ShellExpansionOps(val string: String) {
//     def sh: String = Jitl.shellExpansion(string)
//   }
//
//   val boilerplate = "ls -l /tmp/".sh
//
//   def main(args: Array[String]): Unit = {
//     println(boilerplate)
//   }
// }
//
