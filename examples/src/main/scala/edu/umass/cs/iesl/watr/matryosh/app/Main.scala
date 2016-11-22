package edu.umass.cs.iesl.watr
package matryosh
package app

object AspectMain {



  def main(args: Array[String]) = {
    import Global._
    import experiment._

    filters ++= args

    val examples = new ExprExamples
    val examples2 = new SpecExamples1
    // println("=========================")
    // printAndReset()


  }
}
