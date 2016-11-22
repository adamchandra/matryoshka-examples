package edu.umass.cs.iesl.watr
package matryosh
package experiment


class ExprExamples  {
  import SpecFunctions._

  import app.NamedTransforms._
  import app.Global._

  import scalaz.{Apply => _, _}
  import Scalaz._

  import matryoshka._
  import matryoshka.data._
  import matryoshka.implicits._

  import Exp._
  import Exp2._
  val exp2 = Exp2


  def applyTransInOriginalSpace (): Unit = {
    mul(num(1), mul(num(12), num(8)))
      .prepro(MinusThree, example1ƒ)
    //   mul(num(-1), mul(num(7), num(3)))
  }



  // def gprepro[W[_]: Comonad, A](t: T)(
  //   k: DistributiveLaw[Base, W],
  //   e: Base ~> Base,
  //   f: GAlgebra[W, Base, A])(

  object gprepro {

    def multiplyOriginalWithIdentity(): Unit =  {
      val dh0 = namedDistLaw[Exp, Cofree[Exp, ?]](distHisto)
      val nt0 = namedNatTrans(NaturalTransformation.refl[Exp])
      val na0 = namedGAlgebra[Cofree[Exp, ?], Exp, Fix[Exp]](partialEval[Fix[Exp]] _)

      lam('meh, mul(vari('meh), mul(num(10), num(8))))
        .gprepro[Cofree[Exp, ?], Fix[Exp]](dh0, nt0, na0)

      // equal(lam('meh, mul(vari('meh), num(80))))
    }

    def applyNatRepeatedly(): Unit = {
      lam('meh, mul(vari('meh), mul(num(13), num(8))))
        .gprepro[Cofree[Exp, ?], Fix[Exp]](
        distHisto, MinusThree, partialEval[Fix[Exp]])

      // equal(lam('meh, mul(vari('meh), num(-4))))
    }
  }


  "Recursive" >> {
    "isLeaf" >> {
      "be true for simple literal" in {
        num(1).isLeaf // must beTrue
        num(1).convertTo[Mu[Exp]].isLeaf // must beTrue
        num(1).convertTo[Nu[Exp]].isLeaf // must beTrue
      }

      "be false for expression" in {
        mul(num(1), num(2)).isLeaf // must beFalse
        mul(num(1), num(2)).convertTo[Mu[Exp]].isLeaf // must beFalse
        mul(num(1), num(2)).convertTo[Nu[Exp]].isLeaf // must beFalse
      }
    }

    //   "children" >> {
    //     "be empty for simple literal" in {
    //       num(1).children must be empty;
    //       num(1).convertTo[Mu[Exp]].children must be empty;
    //       num(1).convertTo[Nu[Exp]].children must be empty
    //     }

    //     "contain sub-expressions" in {
    //       mul(num(1), num(2)).children must equal(List(num(1), num(2)))
    //       mul(num(1), num(2)).convertTo[Mu[Exp]].children must
    //         equal(List(num(1), num(2)).map(_.convertTo[Mu[Exp]]))
    //       mul(num(1), num(2)).convertTo[Nu[Exp]].children must
    //         equal(List(num(1), num(2)).map(_.convertTo[Nu[Exp]]))
    //     }
    //   }

    //   "universe" >> {
    //     "be one for simple literal" in {
    //       num(1).universe must equal(List(num(1)))
    //       num(1).convertTo[Mu[Exp]].universe must
    //         equal(List(num(1)).map(_.convertTo[Mu[Exp]]))
    //       num(1).convertTo[Nu[Exp]].universe must
    //         equal(List(num(1)).map(_.convertTo[Nu[Exp]]))
    //     }

    //     "contain root and sub-expressions" in {
    //       mul(num(1), num(2)).universe must
    //         equal(List(mul(num(1), num(2)), num(1), num(2)))
    //       mul(num(1), num(2)).convertTo[Mu[Exp]].universe must
    //         equal(List(mul(num(1), num(2)), num(1), num(2)).map(_.convertTo[Mu[Exp]]))
    //       mul(num(1), num(2)).convertTo[Nu[Exp]].universe must
    //         equal(List(mul(num(1), num(2)), num(1), num(2)).map(_.convertTo[Nu[Exp]]))
    //     }
    //   }

    "transCata" >> {
      "change simple literal" in {
        num(1).transCata(addOneƒ) // must equal(num(2).convertTo[T[Exp]])
      }

      "change sub-expressions" in {
        mul(num(1), num(2)).transCata(addOneƒ) // must equal(mul(num(2), num(3)).convertTo[T[Exp]])
      }

      "be bottom-up" in {
        mul(num(0), num(1)).transCata(addOneOrSimplifyƒ)  // must equal(num(2))) and
        mul(num(1), num(2)).transCata(addOneOrSimplifyƒ) // must equal(mul(num(2), num(3))))
      }
    }

    "transAna" >> {
      "change simple literal" in {
        num(1).transAna(addOneƒ) // must equal(num(2).convertTo[T[Exp]])
      }

      "change sub-expressions" in {
        mul(num(1), num(2)).transAna(addOneƒ) // must equal(mul(num(2), num(3)).convertTo[T[Exp]])
      }

      "be top-down" in {
        mul(num(0), num(1)).transAna(addOneOrSimplifyƒ) // must equal(num(0))
        mul(num(1), num(2)).transAna(addOneOrSimplifyƒ) // must equal(num(2))
      }
    }

    "prepro" >> {
      "multiply original with identity ~>" in {
        mul(num(1), mul(num(12), num(8)))
          .prepro(NaturalTransformation.refl[Exp], example1ƒ) // must equal(96.some)
      }

      "apply ~> repeatedly" in {
        mul(num(1), mul(num(12), num(8))).prepro(MinusThree, example1ƒ) // must equal(-24.some)
      }
    }

    "gprepro" >> {
      "multiply original with identity ~>" in {
        lam('meh, mul(vari('meh), mul(num(10), num(8))))
          .gprepro[Cofree[Exp, ?], Fix[Exp]](
          distHisto, NaturalTransformation.refl[Exp], partialEval[Fix[Exp]])

        // must equal(lam('meh, mul(vari('meh), num(80))))
      }

      "apply ~> repeatedly" in {
        lam('meh, mul(vari('meh), mul(num(13), num(8))))
          .gprepro[Cofree[Exp, ?], Fix[Exp]](distHisto, MinusThree, partialEval[Fix[Exp]])

        // must equal(lam('meh, mul(vari('meh), num(-4))))
      }
    }

    "transPrepro" >> {
      "change literal with identity ~>" in {
        num(1).transPrepro(NaturalTransformation.refl[Exp], addOneƒ)
        // must equal(num(2).convertTo[T[Exp]])
      }

      "apply ~> in original space" in {
        mul(num(1), mul(num(12), num(8))).transPrepro(MinusThree, addOneƒ)
        // equal(mul(num(-1), mul(num(7), num(3))).convertTo[T[Exp]])
      }

      "apply ~> with change of space" in {
        num(1).transPrepro(MinusThree, addOneExpExp2ƒ)
        // equal(num2(2).convertTo[T[Exp2]])
      }
    }

    "transPostpro" >> {
      "change literal with identity ~>" in {
        num(1).transPostpro(NaturalTransformation.refl[Exp], addOneƒ)
        // equal(num(2).convertTo[T[Exp]])
      }

      "apply ~> in original space" in {
        mul(num(1), mul(num(12), num(8))).transPostpro(MinusThree, addOneƒ)
        // equal(mul(num(-1), mul(num(7), num(3))).convertTo[T[Exp]])
      }

      "apply ~> with change of space" in {
        num2(1).transPostpro(MinusThree, addOneExp2Expƒ)
        // equal(num(2).convertTo[T[Exp]])
      }
    }

    "transPara" >> {
      "project basic exp" in {
        lam('sym, num(3)).transPara(extractLambdaƒ) // must equal(num2(3))
      }

      "project basic exp recursively" in {
        lam('sym, mul(num(5), num(7))).transPara(extractLambdaƒ)
        // must equal(single(const))
      }
    }

    "foldMap" >> {
      "fold stuff" in {
        mul(num(0), num(1)).foldMap(_ :: Nil)
        // must equal(mul(num(0), num(1)) :: num(0) :: num(1) :: Nil)
      }
    }

    val findConstants: Exp[List[Int]] => List[Int] = {
      case Num(x) => x :: Nil
      case t      => t.fold
    }

    "cata" >> {
      "evaluate simple expr" in {
        mul(num(1), mul(num(2), num(3))).cata(eval)
        // must equal(6) })
      }

      "find all constants" in {
        mul(num(0), num(1)).cata(findConstants) //  must equal(List(0, 1))
      }

      "produce correct annotations for 5 * 2" in {
        mul(num(5), num(2)).cata(example1ƒ) // must beSome(10)
      }
    }

    "zipAlgebras" >> {
      "both eval and find all constants" in {
        mul(num(5), num(2)).cata(AlgebraZip[Exp].zip(eval, findConstants))
        // must equal((10, List(5, 2)))
      }
    }

    "generalize" >> {
      // "behave like cata" in {
      //   mul(num(1), mul(num(2), num(3))).para(eval.generalize[(T, ?)])
      //   // must equal(t.cata(eval))
      // }
    }

    "coelgot" >> {
      // "behave like cofCata ⋘ attributeAna" >> {
      //   // For any i:Int
      //   val i:Int = 3
      //   i.coelgot(eval.generalizeElgot[(Int, ?)], extractFactors)
      //   // must equal(
      //   i.ana[Cofree[Exp, Int]](attributeCoalgebra(extractFactors)).cata(liftT(eval.generalizeElgot[(Int, ?)]))
      // }
    }

    "elgot" >> {
      // "behave like interpCata ⋘ freeAna" >>  {
      //   // For any i:Int
      //   val i:Int = 3
      //   i.elgot(eval, extractFactors.generalizeElgot[Int \/ ?])
      //   // must equal(
      //   i.ana[Free[Exp, Int]](runT(extractFactors.generalizeElgot[Int \/ ?])).cata(patterns.recover(eval))
      // }
    }

    // For any i:Int
    val i:Int = 3

    "generalizeElgot" >> {
      // "behave like cata on an algebra" {
      //   val x = i.ana[Fix[Exp]](extractFactors).cata(eval)
      //   i.coelgot(eval.generalizeElgot[(Int, ?)], extractFactors) must equal(x)
      // }

      // "behave like ana on an coalgebra" {
      //   val x = i.ana[Fix[Exp]](extractFactors).cata(eval)
      //   i.elgot(eval, extractFactors.generalizeElgot[Int \/ ?]) must equal(x)
      // }
    }

    def extractFactors: Coalgebra[Exp, Int] = x =>
    if (x > 2 && x % 2 == 0) Mul(2, x/2)
    else Num(x)

    //   "generalizeCoalgebra" >> {
    //     "behave like ana" ! prop { (i: Int) =>
    //       i.apo[Fix[Exp]](extractFactors.generalize[Fix[Exp] \/ ?]) must
    //         equal(i.ana[Fix[Exp]](extractFactors))
    //       i.apo[Mu[Exp]](extractFactors.generalize[Mu[Exp] \/ ?]) must
    //         equal(i.ana[Mu[Exp]](extractFactors))
    //       i.apo[Nu[Exp]](extractFactors.generalize[Nu[Exp] \/ ?]) must
    //         equal(i.ana[Nu[Exp]](extractFactors))
    //     }
    //   }
  }
}
