package edu.umass.cs.iesl.watr
package matryosh
package experiment


class ElgotExamples  {
  import SpecFunctions._

  // import app.NamedTransforms._
  import app.Global._
  import ScalazTreeImplicits._
  import scalaz.{Apply => _, _}
  import Scalaz._

  import matryoshka._
  import matryoshka.data._
  import matryoshka.implicits._
  import matryoshka.patterns.EnvT

  import Exp._


  "attributeElgotM" >> {
    "fold to Cofree" in {

      Cofree[Exp, Int](1, Mul(
        Cofree[Exp, Int](2, Num(1)),
        Cofree[Exp, Int](2, Mul(
          Cofree[Exp, Int](3, Num(2)),
          Cofree[Exp, Int](3, Num(3))))))
        .cataM(liftTM(attributeElgotM[(Int, ?), Option](weightedEval)))

      // equal(
      //   Cofree[Exp, Int](216, Mul(
      //     Cofree(2, Num(1)),
      //     Cofree(108, Mul(
      //       Cofree(6, Num(2)),
      //       Cofree(9, Num(3)))))).some)
    }
  }


  // type Coalgebra       [            F[_], A]    = A       => F[A]
  // type CoalgebraM      [      M[_], F[_], A]    = A       => M[F[A]]
  // type ElgotCoalgebra  [E[_],       F[_], A]    = A       => E[F[A]]
  // type ElgotCoalgebraM [E[_], M[_], F[_], A]    = A       => M[E[F[A]]]

  // def generalizeElgot[N[_]: Applicative]: ElgotCoalgebra[N, F, A] = self(_).point[N]
  // def generalizeElgot(coalg: A => F[A]): A => E[F[A]]


  // type Algebra         [            F[_], A]    = F[A]      => A
  // type AlgebraM        [      M[_], F[_], A]    = F[A]      => M[A]
  // type ElgotAlgebra    [W[_],       F[_], A]    = W[F[A]]   => A
  // type ElgotAlgebraM   [W[_], M[_], F[_], A]    = W[F[A]]   => M[A]

  // def generalizeElgot[W[_]: Comonad]: ElgotAlgebra[W, F, A] = w => self(w.copoint)
  // def generalizeElgot(alg: F[A] => A): W[F[A]] => A

  "annotate and fold" >> {
    val RC = implicitly[Recursive.Aux[Cofree[Exp, Int], EnvT[Int, Exp, ?]]]

    val res = 100.elgotApo[Fix[Exp]](extract2sAnd5[Fix[Exp]])
    val rbox = boxTF[Fix[Exp], Exp](res)
    println(s"${rbox}")

    val res2 = res.attributeTopDownM[State[Int, ?], Int](0)(sequential).eval(0)
    // val rbox2 = boxTF[Fix[Exp], Exp](res2)
    val rbox2 = RC.cata(res2)(toTree).drawBox
    println(s"${rbox2}")

    // val res3 = res2.cataM(liftTM(attributeElgotM[(Int, ?), Option](weightedEval)))
    val res3 = res2.cataM(liftTM(attributeElgotM[(Int, ?), Option](weightedEval)))

    res3.foreach { r =>
      // envT[Cofree](r)
      val cbox = RC.cata(r)(toTree).drawBox
      // val box = boxTF[Cofree[Exp, Int], Exp](r)
      println(s"${cbox}")
    }
  }

  "coelgot-refolds" >> {
    // For any i:Int

    "behave like interpCata ⋘ freeAna" >>  {
      val extractElCoalg: ElgotCoalgebra[Int \/ ?, Exp, Int] = extractFactors.generalizeElgot[Int \/ ?]

      "direct" in {
        val res = 8.elgot(eval, extractElCoalg)
        println(s"res: ${res}")
      }

      "via ana" in {
        4.ana[Free[Exp, Int]](runT(extractElCoalg)).cata(patterns.recover(eval))
      }
    }
  }

  "coelgot" >> {
    "behave like cofCata ⋘ attributeAna" >> {
      val genEval = eval.generalizeElgot[(Int, ?)]
      "direct" in {
        val res = 4.coelgot(genEval, extractFactors)
        println(s"res: ${res}")
      }

      "via ana" in {
        8.ana[Cofree[Exp, Int]](attributeCoalgebra(extractFactors)).cata(liftT(genEval))
      }
    }
  }



  "generalizeElgot" >> {
    "behave like cata on an algebra" {
      val x = 10.ana[Fix[Exp]](extractFactors).cata(eval)
      10.coelgot(eval.generalizeElgot[(Int, ?)], extractFactors)
    }

    "behave like ana on an coalgebra" {
      val x = 10.ana[Fix[Exp]](extractFactors).cata(eval)
      10.elgot(eval, extractFactors.generalizeElgot[Int \/ ?])
    }
  }

  "generalizeCoalgebra" >> {
    "behave like ana" in {
      val i = 10
      "one" in {
        i.apo[Fix[Exp]](extractFactors.generalize[Fix[Exp] \/ ?])
      }

      "two" in {
        i.ana[Fix[Exp]](extractFactors)
      }
      // i.apo[Mu[Exp]](extractFactors.generalize[Mu[Exp] \/ ?])
      // i.ana[Mu[Exp]](extractFactors)
      // i.apo[Nu[Exp]](extractFactors.generalize[Nu[Exp] \/ ?])
      // i.ana[Nu[Exp]](extractFactors)
    }
  }

  "behave like elgotApo+elgotAna" >> {
    "one" in {
      val res = 100.elgotAna[Fix[Exp]](distApo[Fix[Exp], Exp], extract2sAnd5[Fix[Exp]])
      println(s"res:\n${boxTF[Fix[Exp], Exp](res)}")
    }

    "two" in {
      val res = 100.elgotApo[Fix[Exp]](extract2sAnd5[Fix[Exp]])
      println(s"res:\n${boxTF[Fix[Exp], Exp](res)}")
    }
  }

  "elgotApo" >> {
    "pull out factors of two and stop on 5" in {

      val res = 420.elgotApo[Fix[Exp]](extract2sAnd5[Fix[Exp]])

      val rbox = boxTF[Fix[Exp], Exp](res)
      println(s"res: ${rbox}")


      // must equal(mul(num(2), mul(num(2), mul(num(5), num(21)))))
    }
  }
}
