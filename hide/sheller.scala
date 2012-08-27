import improving.jitl._

object Sheller {
  final val boilerplate = "uname -a".sh

  def main(args: Array[String]): Unit = println(boilerplate)
}

/**
public final class Sheller$
  SourceFile: "sheller.scala"
    Scala: length = 0x0
     
  #37 = Utf8               Darwin bud.local 11.4.0 Darwin Kernel Version 11.4.0: Mon Apr  9 19:32:15 PDT 2012; root:xnu-1699.26.8~1/RELEASE_X86_64 x86_64
  #38 = String             #37            //  Darwin bud.local 11.4.0 Darwin Kernel Version 11.4.0: Mon Apr  9 19:32:15 PDT 2012; root:xnu-1699.26.8~1/RELEASE_X86_64 x86_64
{

private Sheller$();
    stack=2, locals=1, args_size=1
       0: aload_0       
       1: invokespecial #34                 // Method java/lang/Object."<init>":()V
       4: aload_0       
       5: putstatic     #36                 // Field MODULE$:LSheller$;
       8: aload_0       
       9: ldc           #38                 // String Darwin bud.local 11.4.0 Darwin Kernel Version 11.4.0: Mon Apr  9 19:32:15 PDT 2012; root:xnu-1699.26.8~1/RELEASE_X86_64 x86_64
      11: putfield      #17                 // Field boilerplate:Ljava/lang/String;
      14: return        
**/
