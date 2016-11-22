package edu.umass.cs.iesl.watr
package matryosh
package app

import org.aspectj.lang._
import org.aspectj.lang.annotation._
import textboxing.{TextBoxing => TB}
import TB._
import scala.collection.mutable

object Global {

  val callStack  = mutable.Stack[String]()

  def formatCallStack(): TB.Box = {
    vjoins()(callStack.map(_.box))
  }

  import scala.collection.mutable
  val filters = mutable.ArrayBuffer[String]()

  implicit class FoldEnrichString(val theString: String) extends AnyVal {
    def >>(block: => Unit) = {
      if (filters.exists { f => theString.toLowerCase.contains(f.toLowerCase) }) {
        println(s"running ${theString}")
        block
      }
    }

    def in(block: => Unit) = {
      if (filters.exists { f => theString.toLowerCase.contains(f.toLowerCase) }) {
        println(s"running ${theString}")
        block
      }
    }
  }

}

@Aspect
class MatryoshkaAspects {

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
  // !within(edu.umass.cs.iesl.watr.matryosh.experiment.Global$) &&
  //   !within(edu.umass.cs.iesl.watr.matryosh.experiment.Global) &&
  //   !call(* edu.umass.cs.iesl.watr.matryosh.experiment.Global$.*.*(..)) &&
  //   !execution(* edu.umass.cs.iesl.watr.matryosh.experiment.Global$.*.*(..)) &&
  //   !call(* edu.umass.cs.iesl.watr.matryosh.experiment.AspectMain$.*.*(..)) &&
  //   !within(edu.umass.cs.iesl.watr.matryosh.experiment.AspectMain$) &&
  //   !within(edu.umass.cs.iesl.watr.matryosh.experiment.AspectMain) &&
  //   !call(* *$init$(..)) &&
  // !within(edu.umass.cs.iesl.watr.matryosh.experiment.WatrAspects) &&
  // !within(edu.umass.cs.iesl.watr.textboxing..*) &&
  // !within(edu.umass.cs.iesl.watr.matryosh.app.*) &&
    // !within(edu.umass.cs.iesl.watr.matryosh.app..*) &&

  @Pointcut("""
!within(edu.umass.cs.iesl.watr.textboxing..*) &&
!within(edu.umass.cs.iesl.watr.matryosh.app..*) &&
!call(* edu.umass.cs.iesl.watr.matryosh.app..*.*(..)) &&
!execution(* edu.umass.cs.iesl.watr.matryosh.app..*.*(..)) &&
!execution(* edu.umass.cs.iesl.watr.matryosh.experiment.Exp$Mul.*(..)) &&
!execution(* edu.umass.cs.iesl.watr.matryosh.experiment.Exp$Num.*(..)) &&
!within(matryoshka.data.Fix$) &&
!within(matryoshka.data.Fix) &&
!cflow(execution(String *.toString())) &&
!cflow(call(String *.toString())) &&
!within(java..*) &&
!within(scala..*) &&
!within(scalaz..*) &&
!execution(* scalaz..*.*(..)) &&
!call(* scalaz..*.*(..)) &&
!call(* scala..*.*(..)) &&
!call(* *..*.traverseImpl..*.*(..)) &&
!execution(* *..*.traverseImpl..*.*(..)) &&
!within(org.aspectj..*.*) && (
  execution(* *(..)) ||
  call(* *(..))
)
""") def pointcut1() = {}



  import Global._

  @Around("pointcut1()")
  def around_pointcut1(jp: ProceedingJoinPoint): Any = {

    val stp = jp.getStaticPart
    val stSrcLoc = stp.getSourceLocation
    val srcLoc = jp.getSourceLocation
    val sig = jp.getSignature

    val args = jp.getArgs.map(_.toString()).mkString(", ")

    val stn =  sig.getDeclaringTypeName
      .replaceAll("edu.umass.cs.iesl.watr.matryosh.experiment.", "")
      .replaceAll("edu.umass.cs.iesl.watr.", "")
      .replaceAll("matryoshka.", "")

    val sm =  sig.getModifiers
    val sn =  sig.getName
    val preTrace = s"""${stn}.${sn}(${args})  ${srcLoc}"""


    Global.callStack.push(preTrace)

    ///// Proceed
    val ret = jp.proceed(jp.getArgs)



    // println(s"$ind=> ${ret}")
    // val topFn = s"${ret} <- ${pre}".box
    val retBox = ret.toString
    val pad = " "*(50-retBox.length)
    val currStack = retBox.box + pad + " <- " + formatCallStack()
    println(currStack)
    println("\n\n")

    val pre = Global.callStack.pop()
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
