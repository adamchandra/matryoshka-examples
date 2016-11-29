package edu.umass.cs.iesl.watr
package matryosh
package app

import org.aspectj.lang._
import textboxing.{TextBoxing => TB}
import TB._
import scala.collection.mutable

object Global {

  val callStack  = mutable.Stack[String]()

  def formatCallStack(): TB.Box = {
    vjoins()(
      callStack.reverse.zipWithIndex
        .map({case (frame, i) => s"$i. $frame".box})
        .reverse
    )
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


import ammonite.{ops => fs}
import fs._


object Testing {

  val instNameCache = mutable.HashMap[String, String]()
  // val funcNameCache = mutable.HashMap[Int, String]()
  val sourceFileCache = mutable.HashMap[String, Option[Seq[String]]]()
  val instNumRe = "@[a-z0-9]{6,8}".r

  def cacheFuncName(factoryFn: String, o: Any): Option[String] = {
    val argstr = o.toString()
    val hash = o.hashCode.toString
    if (argstr.startsWith("<function")) {
      instNameCache.getOrElseUpdate(hash, {
        // println(s"caching function: ${factoryFn}@${hash}")
        factoryFn
      })
    }

    instNameCache.get(hash)
  }
  def cacheInst(factoryFn: String, o: Any): Option[String] = {
    val argstr = o.toString()

    instNumRe.findFirstIn(argstr)
      .map({idstr =>
        instNameCache.getOrElseUpdate(idstr, factoryFn)
      })
  }

  def getCachedInst(o: Any): Option[String] = {
    if (o.toString().startsWith("<function")) {
      // println(s"want cached function: ${o.hashCode().toString()}")
    }
    val hash = o.hashCode.toString
    instNameCache.get(hash)
      .orElse({
        instNumRe.findFirstIn(o.toString)
          .flatMap(instNameCache.get(_))
      })
  }

  def reformatObject(o: Any): String = {
    getCachedInst(o) getOrElse {
      if (o.toString().startsWith("<function")) {
        s"ƒ:${o.hashCode.toString}"
      } else {
        o.toString
          .replaceAll("Fix", "_")
      }

    }
  }


  def testingAdviceBefore(jp: JoinPoint, encJP: JoinPoint.StaticPart): Any = {
    val stp = jp.getStaticPart
    val stSrcLoc = stp.getSourceLocation
    val srcLoc = jp.getSourceLocation
    val sig = jp.getSignature
    val encJp = encJP.toShortString()
    val encSL = encJP.getSourceLocation


    val diffSrcs = if (srcLoc.getFileName != encSL.getFileName || srcLoc.getLine != encSL.getLine) {
      s""" !!f${srcLoc.getFileName}/ef:${encSL.getFileName}@ l${srcLoc.getLine}/el:${encSL.getLine}"""
    } else ""

    val args = jp.getArgs.map(reformatObject(_)).mkString(", ")

    val stn =  sig.getDeclaringTypeName
      .replaceAll("edu.umass.cs.iesl.watr.matryosh.experiment.", "")
      .replaceAll("edu.umass.cs.iesl.watr.", "")
      .replaceAll("matryoshka.", "m.")

    val sm =  sig.getModifiers
    val sn =  sig.getName
    val kind = jp.getKind match {
      case "method-execution" => "exec"
      case "method-call" => "call"
      case _ => "?"
    }

    val lineNum = srcLoc.getLine
    val srcCode = if (lineNum > 0) {
      val maybeSrcLines = sourceFileCache.getOrElseUpdate(
        srcLoc.getFileName,
        fs.ls.rec(pwd)
          .filter(_.name.endsWith(srcLoc.getFileName))
          .headOption.map({ p =>
            read.lines(p)
              .map(_.trim.replaceAll(" +", " "))
          })
      )

      maybeSrcLines
        .map({ lines =>
          lines.drop(lineNum-1).headOption.getOrElse(s"<src:${srcLoc.getFileName}:$lineNum>")
        })
        .getOrElse(s"<src:${srcLoc.getFileName}>")

    } else { s"<src:${srcLoc.getFileName}:$lineNum>" }

    // val preTrace = s"""${kind}: ${srcCode} :: ${sn}(${args}) ${sig.toShortString()}"""
    // val preTrace = s"""${kind}: ${srcCode} :: ${sn}(${args}) ${stn}"""
    // val preTrace = s"""${kind}: ${srcCode} :: ${sn}(${args}) ${diffSrcs}"""
    val preTrace = s"""${kind}: ${srcCode} :: ${sn}(${args})"""

    Global.callStack.push(preTrace)

  }

  def testingAdviceAfter(jp: JoinPoint, ret: Any): Unit = {

    val retBox = if (ret!=null) {
      cacheFuncName(jp.getSignature.getName, ret)
      cacheInst(jp.getSignature.getName, ret)
      reformatObject(ret)
    } else "<null>"

    // val currStack = retBox.box + "   <<"  % indent(10)(Global.formatCallStack())
    // println(currStack)
    // println("\n\n")

    val pre = Global.callStack.pop()
  }

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
