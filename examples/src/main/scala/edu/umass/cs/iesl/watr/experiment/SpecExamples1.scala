package edu.umass.cs.iesl.watr
package experiment


// import scala.{_ => _}
// import scala.Predef{_ => _}
// import scala.{Boolean, Function, Int, None, Option, Predef, Symbol, Unit}
// import scala.collection.immutable.{List, Map, Nil, ::}

class SpecExamples1  {
  import SpecFunctions._
  // import NamedTransforms._
  import Global._
  import matryoshka._
  import matryoshka.data._
  import Exp._
  import Exp2._
  val exp2 = Exp2

  import matryoshka.implicits._

  import scalaz.{Apply => _, _}
  import Scalaz._
  // import scalaz.~>
  // import scalaz.Cofree
  // import scalaz.NaturalTransformation
  // import scalaz.std.option._
  // import scalaz.syntax.apply._
  // import scalaz.std.list._

    "topDownCata" >> {

      "bind vars" in {
        val v = let('x, num(1), mul(num(0), vari('x)))
        v.topDownCata(Map.empty[Symbol, Fix[Exp]])(subst)
          // must equal(mul(num(0), num(1)))

        v.convertTo[Mu[Exp]].topDownCata(Map.empty[Symbol, Mu[Exp]])(subst)
          // must equal(mul(num(0), num(1)).convertTo[Mu[Exp]])

        v.convertTo[Nu[Exp]].topDownCata(Map.empty[Symbol, Nu[Exp]])(subst)
          // must equal(mul(num(0), num(1)).convertTo[Nu[Exp]])
      }
    }


    "attributePara" >> {
      "provide a catamorphism" in {
        val v = mul(num(4), mul(num(2), num(3)))
        v.cata(attributePara(peval[Fix[Exp]]))
          // equal(
          //   Cofree[Exp, Int](24, Mul(
          //     Cofree(4, Num(4)),
          //     Cofree(6, Mul(
          //       Cofree(2, Num(2)),
          //       Cofree(3, Num(3)))))))
      }
    }



    "attributeElgotM" >> {
      "fold to Cofree" in {

        attributeElgotM[(Int, ?), Option](weightedEval)
        liftTM(attributeElgotM[(Int, ?), Option](weightedEval))

        Cofree[Exp, Int](1, Mul(
          Cofree[Exp, Int](2, Num(1)),
          Cofree[Exp, Int](2, Mul(
            Cofree[Exp, Int](3, Num(2)),
            Cofree[Exp, Int](3, Num(3))))))
        //   .cataM(liftTM(attributeElgotM[(Int, ?), Option](weightedEval)))

          // equal(
          //   Cofree[Exp, Int](216, Mul(
          //     Cofree(2, Num(1)),
          //     Cofree(108, Mul(
          //       Cofree(6, Num(2)),
          //       Cofree(9, Num(3)))))).some)
        ???
      }
    }

  //   "para" >> {
  //     "evaluate simple expr" in {
  //       testRec(
  //         mul(num(1), mul(num(2), num(3))),
  //         new RecRunner[Exp, Int] {
  //           def run[T](implicit T: Recursive.Aux[T, Exp]) =
  //             _.para(peval[T]) must equal(6)
  //         })
  //     }

  //     "evaluate special-case" in {
  //       testRec(
  //         mul(num(0), num(0)),
  //         new RecRunner[Exp, Int] {
  //           def run[T](implicit T: Recursive.Aux[T, Exp]) =
  //             _.para(peval[T]) must equal(-1)
  //         })
  //     }

  //     "evaluate equiv" in {
  //       testRec(
  //         mul(num(0), mul(num(0), num(1))),
  //         new RecRunner[Exp, Int] {
  //           def run[T](implicit T: Recursive.Aux[T, Exp]) =
  //             _.para(peval[T]) must equal(0)
  //         })
  //     }
  //   }

  //   "gpara" >> {
  //     "behave like para" in {
  //       mul(num(0), mul(num(0), num(1)))
  //         .gpara[Id, Int](distCata, exp => peval(exp.map(_.runEnvT))) must
    //           equal(0)
    //     }
    //   }

    //   def depth[T[_[_]], F[_]]: (Int, F[T[F]]) => Int = (i, _) => i + 1

    //   def sequential[T[_[_]], F[_]]: (Int, F[T[F]]) => State[Int, Int] =
    //     (_, _) => State.get[Int] <* State.modify[Int](_ + 1)

    //   "attributeTopDown" >> {
    //     "increase toward leaves" in {
    //       val v = mul(num(0), mul(num(0), num(1)))
    //       v.attributeTopDown(0)(depth) must equal(
    //         Cofree[Exp, Int](1, Mul(
    //           Cofree(2, Num(0)),
    //           Cofree(2, Mul(
    //             Cofree(3, Num(0)),
    //             Cofree(3, Num(1)))))))
    //     }

    //     "increase toward leaves, ltr" in {
    //       val v = mul(num(0), mul(num(0), num(1)))
    //       v.attributeTopDownM[State[Int, ?], Int](0)(sequential).eval(0) must
    //         equal(
    //           Cofree[Exp, Int](0, Mul(
    //             Cofree(1, Num(0)),
    //             Cofree(2, Mul(
    //               Cofree(3, Num(0)),
    //               Cofree(4, Num(1)))))))
    //     }
    //   }

    //   "distCata" >> {
    //     "behave like cata" in {
    //       val v = mul(num(0), mul(num(0), num(1)))
    //       v.gcata[Id, Int](distCata, eval) must equal(v.cata(eval))
    //       v.convertTo[Mu[Exp]].gcata[Id, Int](distCata, eval) must equal(v.cata(eval))
    //       v.convertTo[Nu[Exp]].gcata[Id, Int](distCata, eval) must equal(v.cata(eval))
    //     }
    //   }

    //   "distPara" >> {
    //     "behave like para" in {
    //       val v = mul(num(0), mul(num(0), num(1)))
    //       v.gcata[(Fix[Exp], ?), Int](distPara, peval[Fix[Exp]]) must equal(v.para(peval[Fix[Exp]]))
    //       v.convertTo[Mu[Exp]].gcata[(Mu[Exp], ?), Int](distPara, peval[Mu[Exp]]) must equal(v.convertTo[Mu[Exp]].para(peval[Mu[Exp]]))
    //       v.convertTo[Nu[Exp]].gcata[(Nu[Exp], ?), Int](distPara, peval[Nu[Exp]]) must equal(v.convertTo[Nu[Exp]].para(peval[Nu[Exp]]))
    //     }
    //   }



    //   "apomorphism" >> {
    //     "pull out factors of two" in {
    //       "apoM" in {
    //         "should be some" in {
    //           12.apoM[Fix[Exp]](extract2sNot5[Fix[Exp]]) must
    //             beSome(mul(num(2), mul(num(2), num(3))))
    //         }
    //         "should be none" in {
    //           10.apoM[Fix[Exp]](extract2sNot5[Fix[Exp]]) must beNone
    //         }
    //       }
    //       "apo should be an optimization over apoM and be semantically equivalent" >> prop { i: Int =>
    //         if (i == 0) ok
    //         else
    //           i.apoM[Fix[Exp]].apply[Id, Exp](extract2s) must
    //             equal(i.apo[Fix[Exp]](extract2s[Fix[Exp]]))
    //       }
    //     }
    //     "construct factorial" in {
    //       4.apo[Fix[Exp]](fact[Fix[Exp]]) must
    //         equal(mul(num(4), mul(num(3), mul(num(2), num(1)))))
    //     }
    //   }

    //   "elgotApo" >> {
    //     "pull out factors of two and stop on 5" in {
    //       420.elgotApo[Fix[Exp]](extract2sAnd5[Fix[Exp]]) must
    //         equal(mul(num(2), mul(num(2), mul(num(5), num(21)))))
    //     }
    //   }

    //   "anamorphism" >> {
    //     "pull out factors of two" in {
    //       "anaM" >> {
    //         def extractFactorsM(x: Int): Option[Exp[Int]] =
    //           if (x == 5) None else extractFactors(x).some
    //         "pull out factors of two" in {
    //           testCorec(
    //             12,
    //             new CorecRunner[Option, Exp, Int] {
    //               def run[T](implicit TC: Corecursive.Aux[T, Exp], Eq: Equal[T], S: Show[T]) =
    //                 _.anaM[T](extractFactorsM) must
    //                   equal(mul(num(2), mul(num(2), num(3))).convertTo[T].some)
    //             })
    //         }
    //         "fail if 5 is present" in {
    //           testCorec(
    //             10,
    //             new CorecRunner[Option, Exp, Int] {
    //               def run[T](implicit TC: Corecursive.Aux[T, Exp], Eq: Equal[T], S: Show[T]) =
    //                 _.anaM[T](extractFactorsM) must beNone
    //             })
    //         }
    //       }
    //       "ana should be an optimization over anaM and be semantically equivalent" >> prop { i: Int =>
    //         testCorec(
    //           i,
    //           new CorecRunner[Id, Exp, Int] {
    //             def run[T](implicit TC: Corecursive.Aux[T, Exp], Eq: Equal[T], S: Show[T]) =
    //               _.anaM[T][Id, Exp](extractFactors) must
    //                 equal(i.ana[T](extractFactors))
    //           })
    //       }
    //     }
    //   }

    //   "distAna" >> {
    //     "behave like ana in gana" >> prop { (i: Int) =>
    //       testCorec(
    //         i,
    //         new CorecRunner[Id, Exp, Int] {
    //           def run[T](implicit TC: Corecursive.Aux[T, Exp], Eq: Equal[T], S: Show[T]) =
    //             _.gana[T][Id, Exp](distAna, extractFactors) must
    //               equal(i.ana[T](extractFactors))
    //         })
    //     }

    //     "behave like ana in elgotAna" >> prop { (i: Int) =>
    //       testCorec(
    //         i,
    //         new CorecRunner[Id, Exp, Int] {
    //           def run[T](implicit TC: Corecursive.Aux[T, Exp], Eq: Equal[T], S: Show[T]) =
    //             _.elgotAna[T][Id, Exp](distAna, extractFactors) must
    //               equal(i.ana[T](extractFactors))
    //         })
    //     }
    //   }

    //   "distApo" >> {
    //     "behave like apo in gana" >> prop { (i: Int) =>
    //       (i.gana[Fix[Exp]](distApo[Fix[Exp], Exp], extract2s[Fix[Exp]]) must
    //         equal(i.apo[Fix[Exp]](extract2s[Fix[Exp]]))).toResult and
    //       (i.gana[Mu[Exp]](distApo[Mu[Exp], Exp], extract2s[Mu[Exp]]) must
    //         equal(i.apo[Mu[Exp]](extract2s[Mu[Exp]]))).toResult and
    //       (i.gana[Nu[Exp]](distApo[Nu[Exp], Exp], extract2s[Nu[Exp]]) must
    //         equal(i.apo[Nu[Exp]](extract2s[Nu[Exp]]))).toResult
    //     }

    //     "behave like elgotApo in elgotAna" >> prop { (i: Int) =>
    //       (i.elgotAna[Fix[Exp]](distApo[Fix[Exp], Exp], extract2sAnd5[Fix[Exp]]) must
    //         equal(i.elgotApo[Fix[Exp]](extract2sAnd5[Fix[Exp]]))).toResult and
    //       (i.elgotAna[Mu[Exp]](distApo[Mu[Exp], Exp], extract2sAnd5[Mu[Exp]]) must
    //         equal(i.elgotApo[Mu[Exp]](extract2sAnd5[Mu[Exp]]))).toResult and
    //       (i.elgotAna[Nu[Exp]](distApo[Nu[Exp], Exp], extract2sAnd5[Nu[Exp]]) must
    //         equal(i.elgotApo[Nu[Exp]](extract2sAnd5[Nu[Exp]]))).toResult
    //     }
    //   }

    //   "hylo" >> {
    //     "factor and then evaluate" >> prop { (i: Int) =>
    //       i.hylo(eval, extractFactors) must equal(i)
    //     }
    //   }

    //   "ghylo" >> {
    //     "behave like hylo with distCata/distAna" ! prop { (i: Int) =>
    //       i.ghylo[Id, Id](distCata, distAna, eval, extractFactors) must
    //         equal(i.hylo(eval, extractFactors))
    //     }

    //     "behave like chrono with distHisto/distFutu" ! prop { (i: Int) =>
    //       i.ghylo[Cofree[Exp, ?], Free[Exp, ?]](
    //         distHisto, distFutu, partialEval[Fix[Exp]], extract2and3) must
    //         equal(i.chrono(partialEval[Fix[Exp]], extract2and3))
    //     }
    //   }


    //   "zygo" >> {
    //     "eval and strings" in {
    //       testRec(
    //         mul(mul(num(0), num(0)), mul(num(2), num(5))),
    //         new RecRunner[Exp, String] {
    //           def run[T](implicit T: Recursive.Aux[T, Exp]) =
    //             _.zygo(eval, strings) must
    //               equal("0 (0), 0 (0) (0), 2 (2), 5 (5) (10)")
    //         })
    //     }
    //   }

    //   "paraZygo" >> {
    //     "peval and strings" in {
    //       testRec(
    //         mul(mul(num(0), num(0)), mul(num(2), num(5))),
    //         new RecRunner[Exp, String] {
    //           def run[T](implicit T: Recursive.Aux[T, Exp]) =
    //             _.paraZygo(peval[T], strings) must
    //               equal("0 (0), 0 (0) (-1), 2 (2), 5 (5) (10)")
    //         })
    //     }
    //   }

    //   sealed trait Nat[A]
    //   final case class Z[A]()        extends Nat[A]
    //   final case class S[A](prev: A) extends Nat[A]
    //   object Nat {
    //     implicit val NatTraverse: Traverse[Nat] = new Traverse[Nat] {
    //       def traverseImpl[G[_], A, B](fa: Nat[A])(f: A => G[B])(implicit G: Applicative[G]):
    //           G[Nat[B]] =
    //         fa match {
    //           case Z()  => G.point(Z())
    //           case S(a) => f(a).map(S(_))
    //         }
    //     }
    //   }

    //   "mutu" >> {
    //     val toNat: Int => Fix[Nat] = _.ana[Fix[Nat]]({
    //       case 0 => Z(): Nat[Int]
    //       case n => S(n - 1): Nat[Int]
    //     })

    //     case class Even(even: Boolean)
    //     case class Odd(odd: Boolean)

    //     val isOdd: Nat[(Even, Odd)] => Odd = {
    //       case Z()             => Odd(false)
    //       case S((Even(b), _)) => Odd(b)
    //     }
    //     val isEven: Nat[(Odd, Even)] => Even = {
    //       case Z()            => Even(true)
    //       case S((Odd(b), _)) => Even(b)
    //     }

    //     "determine even" in {
    //       toNat(8).mutu(isOdd, isEven) must_== Even(true)
    //     }

    //     "determine odd" in {
    //       toNat(5).mutu(isEven, isOdd) must_== Odd(true)
    //     }

    //     "determine not even" in {
    //       toNat(7).mutu(isOdd, isEven) must_== Even(false)
    //     }
    //   }

    //   "histo" >> {
    //     "eval simple literal multiplication" in {
    //       mul(num(5), num(10)).histo(partialEval[Fix[Exp]]) must equal(num(50))
    //       mul(num(5), num(10)).histo(partialEval[Mu[Exp]]) must equal(num(50).convertTo[Mu[Exp]])
    //       mul(num(5), num(10)).histo(partialEval[Nu[Exp]]) must equal(num(50).convertTo[Nu[Exp]])
    //     }

    //     "partially evaluate mul in lambda" in {
    //       lam('foo, mul(mul(num(4), num(7)), vari('foo))).histo(partialEval[Fix[Exp]]) must
    //         equal(lam('foo, mul(num(28), vari('foo))))
    //       lam('foo, mul(mul(num(4), num(7)), vari('foo))).histo(partialEval[Mu[Exp]]) must
    //         equal(lam('foo, mul(num(28), vari('foo))).convertTo[Mu[Exp]])
    //       lam('foo, mul(mul(num(4), num(7)), vari('foo))).histo(partialEval[Nu[Exp]]) must
    //         equal(lam('foo, mul(num(28), vari('foo))).convertTo[Nu[Exp]])
    //     }
    //   }

    //   "postpro" >> {
    //     "extract original with identity ~>" in {
    //       (72.postpro[Fix[Exp]](NaturalTransformation.refl[Exp], extractFactors) must
    //         equal(mul(num(2), mul(num(2), mul(num(2), num(9)))))).toResult and
    //       (72.postpro[Mu[Exp]](NaturalTransformation.refl[Exp], extractFactors) must
    //         equal(mul(num(2), mul(num(2), mul(num(2), num(9)))).convertTo[Mu[Exp]])).toResult and
    //       (72.postpro[Nu[Exp]](NaturalTransformation.refl[Exp], extractFactors) must
    //         equal(mul(num(2), mul(num(2), mul(num(2), num(9)))).convertTo[Nu[Exp]])).toResult
    //     }

    //     "apply ~> repeatedly" in {
    //       (72.postpro[Fix[Exp]](MinusThree, extractFactors) must
    //         equal(mul(num(-1), mul(num(-4), mul(num(-7), num(0)))))).toResult and
    //       (72.postpro[Mu[Exp]](MinusThree, extractFactors) must
    //         equal(mul(num(-1), mul(num(-4), mul(num(-7), num(0)))).convertTo[Mu[Exp]])).toResult and
    //       (72.postpro[Nu[Exp]](MinusThree, extractFactors) must
    //         equal(mul(num(-1), mul(num(-4), mul(num(-7), num(0)))).convertTo[Nu[Exp]])).toResult
    //     }
    //   }

    //   "gpostpro" >> {
    //     "extract original with identity ~>" in {
    //       72.gpostpro[Fix[Exp]](distFutu[Exp], NaturalTransformation.refl, extract2and3) must
    //         equal(mul(num(2), mul(num(2), mul(num(2), mul(num(3), num(3))))))
    //     }

    //     "apply ~> repeatedly" in {
    //       72.gpostpro[Fix[Exp]](distFutu[Exp], MinusThree, extract2and3) must
    //         equal(mul(num(-1), mul(num(-4), mul(num(-7), mul(num(-9), num(-9))))))
    //     }
    //   }

    //   "futu" >> {
    //     "factor multiples of two" in {
    //       testCorec(
    //         8,
    //         new CorecRunner[Id, Exp, Int] {
    //           def run[T](implicit TC: Corecursive.Aux[T, Exp], Eq: Equal[T], S: Show[T]) =
    //             _.futu[T](extract2and3) must equal(mul(num(2), mul(num(2), num(2))).convertTo[T])
    //         })
    //     }

    //     "factor multiples of three" in {
    //       testCorec(
    //         81,
    //         new CorecRunner[Id, Exp, Int] {
    //           def run[T](implicit TC: Corecursive.Aux[T, Exp], Eq: Equal[T], S: Show[T]) =
    //             _.futu[T](extract2and3) must
    //               equal(mul(num(3), num(27)).convertTo[T])
    //         })
    //     }

    //     "factor 3 within 2" in {
    //       testCorec(
    //         324,
    //         new CorecRunner[Id, Exp, Int] {
    //           def run[T](implicit TC: Corecursive.Aux[T, Exp], Eq: Equal[T], S: Show[T]) =
    //             _.futu[T](extract2and3) must
    //               equal(mul(num(2), mul(num(2), mul(num(3), num(27)))).convertTo[T])
    //         })
    //     }
    //   }

    //   "chrono" >> {
    //     "factor and partially eval" >> prop { (i: Int) =>
    //       i.chrono(partialEval[Fix[Exp]], extract2and3) must equal(num(i))
    //       i.chrono(partialEval[Mu[Exp]], extract2and3) must equal(num(i).convertTo[Mu[Exp]])
    //       i.chrono(partialEval[Nu[Exp]], extract2and3) must equal(num(i).convertTo[Nu[Exp]])
    //     }
    //   }
    // }

    // "Holes" >> {
    //   "holes" >> {
    //     "find none" in {
    //       holes[Exp, Unit](Num(0)) must_=== Num(0)
    //     }

    //     "find and replace two children" in {
    //       (holes(mul(num(0), num(1)).unFix) match {
    //         case Mul((Fix(Num(0)), f1), (Fix(Num(1)), f2)) =>
    //           f1(num(2)) must equal(Mul(num(2), num(1)))
    //           f2(num(2)) must equal(Mul(num(0), num(2)))
    //         case r => failure
    //       }): org.specs2.execute.Result
    //     }
    //   }

    //   "holesList" >> {
    //     "find none" in {
    //       holesList[Exp, Unit](Num(0)) must be empty
    //     }

    //     "find and replace two children" in {
    //       (holesList(mul(num(0), num(1)).unFix) match {
    //         case (t1, f1) :: (t2, f2) :: Nil =>
    //           t1         must equal(num(0))
    //           f1(num(2)) must equal(Mul(num(2), num(1)))
    //           t2         must equal(num(1))
    //           f2(num(2)) must equal(Mul(num(0), num(2)))
    //         case _ => failure
    //       }): org.specs2.execute.Result
    //     }
    //   }

    //   "project" >> {
    //     "not find child of leaf" in {
    //       project(0, num(0).unFix) must beNone
    //     }

    //     "find first child of simple expr" in {
    //       project(0, mul(num(0), num(1)).unFix) must beSome(num(0))
    //     }

    //     "not find child with bad index" in {
    //       project(-1, mul(num(0), num(1)).unFix) must beNone
    //       project(2, mul(num(0), num(1)).unFix) must beNone
    //     }
    //   }
    // }

    // "Attr" >> {
    //   "attrSelf" >> {
    //     "annotate all" >> Prop.forAll(expGen) { exp =>
    //       exp.cata(attrSelf[Mu[Exp], Exp]).universe must
    //         equal(exp.universe.map(_.cata(attrSelf[Mu[Exp], Exp])))
    //     }
    //   }

    //   "convert" >> {
    //     "forget unit" >> Prop.forAll(expGen) { exp =>
    //       exp.cata(attrK(())).cata(deattribute[Exp, Unit, Mu[Exp]](_.embed)) must equal(exp)
    //     }
    //   }

    //   "foldMap" >> {
    //     "zeros" >> Prop.forAll(expGen) { exp =>
    //       Foldable[Cofree[Exp, ?]].foldMap(exp.cata(attrK(0)))(_ :: Nil) must
    //         equal(exp.universe.map(Function.const(0)))
    //     }

    //     "selves" >> Prop.forAll(expGen) { exp =>
    //       Foldable[Cofree[Exp, ?]].foldMap(exp.cata[Cofree[Exp, Mu[Exp]]](attrSelf))(_ :: Nil) must
    //         equal(exp.universe)
    //     }
    //   }
    // }

    // "count" should {
    //   "return the number of instances in the structure" in {
    //     val exp = mul(mul(num(12), mul(num(12), num(8))), mul(num(12), num(8)))
    //     exp.para(count(num(12))) must equal(3)
    //   }
    // }

    // "size" should {
    //   "return the number of nodes in the structure" in {
    //     val exp = mul(mul(num(12), mul(num(12), num(8))), mul(num(12), num(8)))
    //     exp.cata(matryoshka.size) must equal(9)
    //   }
    // }

    // "height" should {
    //   "return the longest path from root to leaf" in {
    //     val exp = mul(mul(num(12), mul(num(12), num(8))), mul(num(12), num(8)))
    //     exp.cata(height) must equal(3)
    //   }
    // }

    // "find" should {
    //   val exp = mul(mul(num(10), mul(num(11), num(7))), mul(num(12), num(8)))

    //   "return root-most instance that passes" in {
    //     exp.transAnaTM(matryoshka.find[Fix[Exp]] {
    //       case Embed(Mul(Embed(Num(_)), _)) => true
    //       case _                            => false
    //     }) must equal(mul(num(10), mul(num(11), num(7))).left)
    //   }

    //   "return leaf-most instance that passes" in {
    //     exp.transCataTM(matryoshka.find[Fix[Exp]] {
    //       case Embed(Mul(Embed(Num(_)), _)) => true
    //       case _                            => false
    //     }) must equal(mul(num(11), num(7)).left)
    //   }
    // }

    // "substitute" should {
    //   "replace equivalent forms" in {
    //     val exp = mul(mul(num(12), mul(num(12), num(8))), mul(num(12), num(8)))
    //     val res = mul(mul(num(12), num(92)), num(92))
    //     exp.transApoT(substitute(mul(num(12), num(8)), num(92))) must equal(res)
    //   }

    //   "replace equivalent forms without re-replacing created forms" in {
    //     val exp = mul(mul(num(12), mul(num(12), num(8))), mul(num(12), num(8)))
    //     val res = mul(mul(num(12), num(8)), num(8))
    //     exp.transApoT(substitute(mul(num(12), num(8)), num(8))) must equal(res)
    //   }

    //   "replace equivalent forms without re-replacing inserted forms" in {
    //     val exp = mul(mul(num(12), num(8)), num(8))
    //     val res = mul(mul(num(12), mul(num(12), num(8))), mul(num(12), num(8)))
    //     exp.transApoT(substitute(num(8), mul(num(12), num(8)))) must equal(res)
    //   }
    // }

    // "recover" should {
    //   import matryoshka.patterns._

    //   "handle “partially-folded” values" in {
    //     val exp =
    //       CoEnv[Int, Exp, Fix[CoEnv[Int, Exp, ?]]](\/-(Mul(
    //         CoEnv[Int, Exp, Fix[CoEnv[Int, Exp, ?]]](\/-(Mul(
    //           CoEnv(2.left[Exp[Fix[CoEnv[Int, Exp, ?]]]]).embed,
    //           CoEnv[Int, Exp, Fix[CoEnv[Int, Exp, ?]]](\/-(Mul(
    //             CoEnv[Int, Exp, Fix[CoEnv[Int, Exp, ?]]](Num(3).right[Int]).embed,
    //             CoEnv(4.left[Exp[Fix[CoEnv[Int, Exp, ?]]]]).embed))).embed))).embed,
    //         CoEnv[Int, Exp, Fix[CoEnv[Int, Exp, ?]]](\/-(Mul(
    //           CoEnv[Int, Exp, Fix[CoEnv[Int, Exp, ?]]](Num(5).right[Int]).embed,
    //           CoEnv(6.left[Exp[Fix[CoEnv[Int, Exp, ?]]]]).embed))).embed))).embed
    //     exp.cata(recover(eval)) must equal(720)
    //   }
}
