package edu.umass.cs.iesl.watr
package matryosh
package experiment

import matryoshka._

import matryoshka.data._
import matryoshka.implicits._

import scalaz.{Apply => _, _}, Scalaz._

class ScalalzExamples  {
  import app.Global._
  import Exp._
  // import Exp2._
  // val exp2 = Exp2

  sealed trait TX
  val TX = Tag.of[TX]
  val br = implicitly[BindRec[(String, String, ?)]]
  // // final def traverse[G[_], B](f: A => G[B])(implicit G: Applicative[G]): G[F[B]] =  G.traverse(self)(f)
  val tlist:(String, String, List[Int]) = List[Option[Int]](Some(1), Some(2), Some(3), Some(4))
    .traverse[(String, String, ?), Int]({ opti =>
      println(s"o: $opti")
      (opti.get.toString, opti.get.toString+"/", opti.get+10)
    })
  println("tlist = " + tlist)

  val olist = List[Option[Int]](Some(1), Some(2), Some(3), Some(4))
  val ilist = List[Int](1, 2, 3, 4)

  def printlni(i: Int)(s: String): Unit = {
    println((" "*(i*4))+s)
  }
  def printi(i: Int)(s: String): Unit = {
    print((" "*(i*4))+s)
  }

  def shortenFix(fs: Any): String = {
    fs.toString
      .replaceAll("\\(", "")
      .replaceAll("\\)", "")
      .replaceAll("Fix", "")
      .replaceAll("Num", "n")
      .replaceAll("Mul", "*")
  }

  def sequential[T[_[_]], F[_]]: (Int, F[T[F]]) => State[Int, Int] =
    (_, _) => for {
      i <- State.get[Int]
      _ <- State.modify[Int](_ + 1)
    } yield i

  def sequentialx: State[Int, Int] = for {
    i <- State.get[Int]
    _ <- State.modify[Int](_ + 1)
  } yield i

  def attrTopDownM(tin: Fix[Exp], z: Int, d: Int)(
    f: State[Int, Int]
  ): State[Int, Cofree[Exp, Int]] = {
    printlni(d-1)(s"mattributeTopDownM(t=${shortenFix(tin)}, z=${z}, depth=${d})")
    val ft = tin.project
    for {
      a <- f
      _ = printlni(d)(s"${a}:a <- f(z=$z, ft=${shortenFix(ft)})")
      _ = printlni(d)(s"""ftx <- $ft.traverse{ """)
      ftx <- ft.traverse[State[Int, ?], Cofree[Exp, Int]]({ tft =>
        printlni(d+1)(s" ${shortenFix((tft))} => ")
        attrTopDownM(tft, a, d+3)(f)
      })
      _ = printlni(d)("}")
      _ = printlni(d)(s"ftx = $ftx")
    } yield {
      val r = Cofree(a, ftx)
      printlni(d)(s"yield Cofree($a, $ftx)")
      r
    }
  }
  // def attributeTopDownM[M[_]: Monad, A](
  //   t: T, z: A)(
  //   f: (A, Base[T]) => M[A])(
  //   implicit BT: Traverse[Base]):
  //     M[Cofree[Base, A]] = {
  //   val ft = project(t)
  //   f(z, ft) >>=
  //     (a => ft.traverse(attributeTopDownM(_, a)(f)) ∘ (Cofree(a, _)))
  // }

  // val LT = implicitly[Traverse[List]]
  // val optList = LT.traverse(ilist)(i => Option(i))



  "scalaz top down exp" >> {
    val v = mul(mul(num(1), num(2)), mul(num(3), num(4)))

    val stFun= attrTopDownM(v, 0, 1)(sequentialx)
    println("Built the state func, eval: ")

    val res = stFun.eval(0)

    println(res.cata(toTree).drawTree)


    // val f = sequential
    // val ft = v.project
    // val qqq = f(0, v) >>= (a =>  {
    //   ft.traverse[State[Int, ?], Cofree[Exp, Int]](ft => {
    //     ft.attributeTopDownM(a)(f)
    //   }) ∘ (Cofree(a, _))
    // })

    // for {
    //   a <- f(0, v)
    //   t <- v.project.traverse[State[Int, ?], Cofree[Exp, Int]](
    //     _.attributeTopDownM(a)(f))
    // } yield Cofree(a, t)
  }
}
