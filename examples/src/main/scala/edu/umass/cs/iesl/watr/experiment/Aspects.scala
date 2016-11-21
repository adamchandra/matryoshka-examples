package edu.umass.cs.iesl.watr
package experiment

import org.aspectj.lang._
import org.aspectj.lang.annotation._

// import textboxing.{TextBoxing => TB} // TB._
import scala.collection.mutable

object Global {
  var indentPad: Int = 0

  val traceStack  = mutable.Stack[String]()
  val ranStack  = mutable.Stack[String]()

  def printAndReset(): Unit = {
    while(!ranStack.isEmpty) {
      println(
        ranStack.pop()
      )
    }

    traceStack.clear()
    indentPad = 0
  }

  import scala.collection.mutable
  val filters = mutable.ArrayBuffer[String]()

  implicit class FoldEnrichString(val theString: String) extends AnyVal {
    def >>(block: => Unit) = {
      if (filters.exists { f => theString.contains(f) }) {
        println(s"running ${theString}")
        block
      }
    }

    def in(block: => Unit) = {
      theString >> block
    }
  }

}

@Aspect
class WatrAspects {

  // @Pointcut("call(* toAtoms(..))")
  // def pcToAtoms() =  {}

  // @Before("pcToAtoms()")
  // def beforePcToAtoms(jp: JoinPoint): Unit = {
  //   println(s"beforePcToAtoms:${jp}")
  // }

  // @Pointcut("within(matryoshka..*)")
  // @Pointcut("execution(* matryoshka..*.*(..))")
  // @Pointcut("call(* matryoshka..*.*(..)) && !within(WatrAspects)")
  // def pointcut0() = {
  // }

  // @Before("pointcut0()")
  // def before_pointcut0(jp: JoinPoint) = println(s"before:pointcut0 ${jp}")


  // @Pointcut("withincode(_root_.matryoshka..*.*(..)) && execution(* *(..)) && !within(WatrAspects)")
  // @Pointcut("cflow(pointcut0()) && execution(* *(..))")
  // !call(* scala..*.*(..)) &&
  //   !call(* scalaz..*.*(..)) &&
  // !execution(* scalaz..*.*(..)) &&
  // !call(* *$eq(..)) &&
  //   !call(* *$init$(..)) &&

  @Pointcut("""
!within(matryoshka.data.Fix$) &&
!within(matryoshka.data.Fix$) &&
!cflow(execution(String *.toString())) &&
!cflow(call(String *.toString())) &&
!within(edu.umass.cs.iesl.watr.textboxing..*) &&
!within(java..*) &&
!within(scala..*) &&
!within(scalaz..*) &&
!execution(* scalaz..*.*(..)) &&
!call(* scalaz..*.*(..)) &&
!call(* scala..*.*(..)) &&
!call(* *..*.traverseImpl..*.*(..)) &&
!execution(* *..*.traverseImpl..*.*(..)) &&
!within(edu.umass.cs.iesl.watr.experiment.WatrAspects) &&
!within(edu.umass.cs.iesl.watr.experiment.Global$) &&
!within(edu.umass.cs.iesl.watr.experiment.Global) &&
!within(edu.umass.cs.iesl.watr.experiment.AspectMain$) &&
!within(edu.umass.cs.iesl.watr.experiment.AspectMain) &&
!within(org.aspectj..*.*) && (
  execution(* *(..))  ||
  call(* *(..))
)
""") def pointcut1() = {}



  @Around("pointcut1()")
  def around_pointcut1(jp: ProceedingJoinPoint): Any = {
    Global.indentPad += 1
    val ind = " " * Global.indentPad*2

    val stp = jp.getStaticPart
    val stSrcLoc = stp.getSourceLocation
    val srcLoc = jp.getSourceLocation
    val sig = jp.getSignature

    val args = jp.getArgs.map(_.toString()).mkString(", ")
    // val preTrace = s"$${jp} ${args} ${srcLoc}"

    // val sdt = sig.getDeclaringType
    val stn =  sig.getDeclaringTypeName
      .replaceAll("edu.umass.cs.iesl.watr.experiment.", "")
      .replaceAll("edu.umass.cs.iesl.watr.", "")
      .replaceAll("matryoshka.", "")

    val sm =  sig.getModifiers
    val sn =  sig.getName
      //   edu.umass.cs.iesl.watr.experiment.Exp$ 1 traverse
    // val preTrace2 = s"${jp} ${args}"
    val preTrace = s"""${stn}.${sn}(${args})"""

    println(s"$ind${preTrace}")

    Global.traceStack.push(preTrace)
    // println(
    //   indent(Global.indent*2)(
    //     s"""|StaticPart
    //         |    getId             ${stp.getId              }
    //         |    getKind           ${stp.getKind            }
    //         |    getSignature      ${stp.getSignature       }
    //         |    getSourceLocation ${stp.getSourceLocation  }
    //         |    toLongString      ${stp.toLongString       }
    //         |    toShortString     ${stp.toShortString      }
    //         |    toString          ${stp.toString           }
    //         |""".stripMargin.mbox
    //   )
    // )
    // val sigStr =
    //   s"""|Signature
    //       |  getDeclaringType    ${ sig.getDeclaringType     }
    //       |  getDeclaringTypeName${ sig.getDeclaringTypeName }
    //       |  getModifiers        ${ sig.getModifiers         }
    //       |  getName             ${ sig.getName              }
    //       |  toLongString        ${ sig.toLongString         }
    //       |  toShortString       ${ sig.toShortString        }
    //       |""".stripMargin

    // println(
    //   s"""|
    //       |getArgs           ${ args           }
    //       |getKind           ${ jp.getKind           }
    //       |${sigStr}
    //       |getSourceLocation ${ jp.getSourceLocation }
    //       |${srcLoc}
    //       |getStaticPart     ${ jp.getStaticPart     }
    //       |toLongString      ${ jp.toLongString      }
    //       |toShortString     ${ jp.toShortString     }
    //       |""".stripMargin
    // )
    val ret = jp.proceed(jp.getArgs)
    val pre = Global.traceStack.pop()

    println(s"$ind=> ${ret}")
    Global.ranStack.push(s"${ind}${ret} <- ${pre}")
    Global.indentPad -= 1
    ret
  }

  // val srcLoc =
  //   s"""|SourceLoc
  //       | getFileName    ${sourceLoc.getFileName  }
  //       | getLine        ${sourceLoc.getLine      }
  //       | getWithinType  ${sourceLoc.getWithinType}
  //       |""".stripMargin
  // @Before("pointcut1()")
  // def before_pointcut1(jp: JoinPoint) = {

  //   println(s"before:pointcut1 ${jp}")

  //   val args = jp.getArgs.map(_.toString()).mkString(", ")
  //   val sig = jp.getSignature
  //   // () => Class[?0]
  //   //   () => String
  //   // () => Int
  //   // () => String
  //   // () => String
  //   // () => String

  //   val sigStr =
  //     s"""|Signature
  //         |  getDeclaringType    ${ sig.getDeclaringType     }  () => Class[?0]
  //         |  getDeclaringTypeName${ sig.getDeclaringTypeName }    () => String
  //         |  getModifiers        ${ sig.getModifiers         }  () => Int
  //         |  getName             ${ sig.getName              }  () => String
  //         |  toLongString        ${ sig.toLongString         }  () => String
  //         |  toShortString       ${ sig.toShortString        }  () => String
  //         |""".stripMargin

  //   // |getSignature      ${ jp.getSignature      }  () => Signature
  //   println(
  //     s"""|
  //         |getArgs           ${ args           }  () => Array[Object]
  //         |getKind           ${ jp.getKind           }  () => String
  //         |${sigStr}
  //         |getSourceLocation ${ jp.getSourceLocation }  () => SourceLocation
  //         |getStaticPart     ${ jp.getStaticPart     }  () => StaticPart
  //         |toLongString      ${ jp.toLongString      }  () => String
  //         |toShortString     ${ jp.toShortString     }  () => String
  //         |""".stripMargin
  //   )
  // }

  // @After("pointcut1()")
  // def after_pointcut1(jp: JoinPoint) =  {
  //   println(s"after:pointcut1 ${jp}")
  // }

  // @Before("pointcut1()")
  // def trace_in_pointcut1(jp: JoinPoint) = {
  // }

  // @After("pointcut1()")
  // def trace_out_pointcut1(jp: JoinPoint) =  {
  //   println(s"after:pointcut1 ${jp}")
  // }
}



object AspectMain {



  def main(args: Array[String]) = {
    import Global._

    filters ++= args

    val examples = new ExprExamples


  }
}
