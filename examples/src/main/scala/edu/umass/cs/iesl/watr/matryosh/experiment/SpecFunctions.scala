package edu.umass.cs.iesl.watr
package matryosh
package experiment

object SpecFunctions {
  // import app.NamedTransforms._

  import scalaz.{Apply => _, _}
  import Scalaz._

  import matryoshka._
  import matryoshka.implicits._

  import Exp._
  import Exp2._
  val exp2 = Exp2

  def subst[T]
    (vars: Map[Symbol, T], t: T)
    (implicit T: Recursive.Aux[T, Exp])
      : (Map[Symbol, T], T) = t.project match {
    case Let(sym, value, body) => (vars + ((sym, value)), body)
    case Var(sym)              => (vars, vars.get(sym).getOrElse(t))
    case _                     => (vars, t)
  }

  val example1ƒ: Exp[Option[Int]] => Option[Int] = {
    case Num(v)           => Some(v)
    case Mul(left, right) => (left ⊛ right)(_ * _)
    case Var(v)           => None
    case Lambda(_, b)     => b
    case Apply(func, arg) => None
    case Let(_, _, i)     => i
  }

  val MinusThree: Exp ~> Exp =
    new (Exp ~> Exp) {
      def apply[A](exp: Exp[A]): Exp[A] = exp match {
        case Num(x) => Num(x-3)
        case t      => t
      }
    }

  // override def toString() = "<f:x-3>"
  // res

  def addOneOptƒ[T[_[_]]]: Exp[T[Exp]] => Option[Exp[T[Exp]]] = t => t match {
    case Num(n) => Num(n+1).some
    case _      => None
  }

  def addOneƒ[T[_[_]]]: Exp[T[Exp]] => Exp[T[Exp]] =
    orOriginal(addOneOptƒ)

  def addOneOptExpExp2ƒ[T[_[_]]]: Exp[T[Exp2]] => Option[Exp2[T[Exp2]]] = {
    case Num(n) => Num2(n+1).some
    case _      => None
  }

  def addOneExpExp2ƒ[T[_[_]]]: Exp[T[Exp2]] => Exp2[T[Exp2]] =
    orDefault[Exp[T[Exp2]], Exp2[T[Exp2]]](exp2.Const())(addOneOptExpExp2ƒ)

  def addOneOptExp2Expƒ[T[_[_]]]: Exp2[T[Exp2]] => Option[Exp[T[Exp2]]] = {
    case Num2(n) => Num(n+1).some
    case _       => None
  }

  def addOneExp2Expƒ[T[_[_]]]: Exp2[T[Exp2]] => Exp[T[Exp2]] =
    orDefault[Exp2[T[Exp2]], Exp[T[Exp2]]](Num(0))(addOneOptExp2Expƒ)

  def simplifyƒ[T[_[_]]: RecursiveT]: Exp[T[Exp]] => Option[Exp[T[Exp]]] = {
    case Mul(a, b) => (a.projectT, b.projectT) match {
      case (Num(0), Num(_)) => Num(0).some
      case (Num(1), Num(n)) => Num(n).some
      case (Num(_), Num(0)) => Num(0).some
      case (Num(n), Num(1)) => Num(n).some
      case (_,      _)      => None
    }
    case _         => None
  }

  def addOneOrSimplifyƒ[T[_[_]]: RecursiveT]: Exp[T[Exp]] => Exp[T[Exp]] = {
    case t @ Num(_)    => addOneƒ(t)
    case t @ Mul(_, _) => repeatedly(simplifyƒ[T]).apply(t)
    case t             => t
  }

  def extractLambdaƒ[T[_[_]]: RecursiveT]: Exp[(T[Exp], T[Exp2])] => Exp2[T[Exp2]] = {
    case Lambda(_, (exp, exp2)) => exp.projectT match {
      case Num(a) => Num2(a)
      case _      => Single(exp2)
    }
    case _                      => exp2.Const[T[Exp2]]
  }



  // NB: This is better done with cata, but we fake it here
  def partialEval[T]
    (t: Exp[Cofree[Exp, T]])
    (implicit TR: Recursive.Aux[T, Exp], TC: Corecursive.Aux[T, Exp])
      : T =
    t match {
      case Mul(x, y) => (x.head.project, y.head.project) match {
        case (Num(a), Num(b)) => Num[T](a * b).embed
        case _                => t.map(_.head).embed
      }
      case _ => t.map(_.head).embed
    }

  def eval: Algebra[Exp, Int] = {
    case Num(x)    => x
    case Mul(x, y) => x * y
    case _         => ???
  }

  // Evaluate as usual, but trap 0*0 as a special case
  def peval[T](t: Exp[(T, Int)])(implicit T: Recursive.Aux[T, Exp]): Int =
    t match {
      case Mul((Embed(Num(0)), _), (Embed(Num(0)), _)) => -1
      case Mul((_,             x), (_,             y)) => x * y
      case Num(x)                                      => x
      case _                                           => ???
    }


  // type ElgotAlgebraM[W[_], M[_], F[_], A] = W[F[A]] => M[A]
  val weightedEval: ElgotAlgebraM[(Int, ?), Option, Exp, Int] = {
    case (weight, Num(x))    => Some(weight * x)
    case (weight, Mul(x, y)) => Some(weight * x * y)
    case (_,      _)         => None
  }

  def extractFactors: Coalgebra[Exp, Int] =
    x => if (x > 2 && x % 2 == 0) Mul(2, x/2) else Num(x)


  // def elgot[F[_]: Functor, A, B](a: A)(φ: Algebra[F, B], ψ: ElgotCoalgebra[B \/ ?, F, A]): B = {
  //   def h: A => B =
  //     (((x: B) => x) ||| ((x: F[A]) => φ(x ∘ h))) ⋘ ψ
  //   h(a)
  // }



  // def extractFactorsM(x: Int): Option[Exp[Int]] =
  def extractFactorsM: CoalgebraM[Option, Exp, Int] =
    (x:Int) => if (x == 5) None else extractFactors(x).some



  def depth[T[_[_]], F[_]]: (Int, F[T[F]]) => Int = (i, _) => i + 1

  def sequential[T[_[_]], F[_]]: (Int, F[T[F]]) => State[Int, Int] =
    (_, _) => State.get[Int] <* State.modify[Int](_ + 1)


  // val weightedEval: ElgotAlgebraM[(Int, ?), Option, Exp, Int] =
  //   namedElgotAlgebraM[(Int, ?), Option, Exp, Int](_weightedEval)

  // val weightedEval: ElgotAlgebraM[(Int, ?), Option, Exp, Int] = namedElgotAlgebraM2({
  //   case (weight, Num(x))    => Some(weight * x)
  //   case (weight, Mul(x, y)) => Some(weight * x * y)
  //   case (_,      _)         => None
  // })

  def extract2s[T](implicit T: Corecursive.Aux[T, Exp])
      : Int => Exp[T \/ Int] = x =>
  if (x == 0) Num(x)
  else if (x % 2 == 0) Mul(-\/(Num[T](2).embed), \/-(x.toInt / 2))
  else Num(x)

  def extract2sAnd5[T](implicit T: Corecursive.Aux[T, Exp])
      : Int => T \/ Exp[Int] = x =>
  if (x <= 2) Num(x).right
  else if (x % 2 == 0) \/-(Mul(2, x / 2))
  else if (x % 5 == 0)
    Mul(Num[T](5).embed, Num[T](x / 5).embed).embed.left
  else Num(x).right

  def extract2sNot5[T](x: Int)(implicit T: Corecursive.Aux[T, Exp]):
      Option[Exp[T \/ Int]] =
    if (x == 5) None else extract2s[T].apply(x).some

  def fact[T](x: Int)(implicit T: Corecursive.Aux[T, Exp]): Exp[T \/ Int] =
    if (x > 1) Mul(-\/(Num[T](x).embed), \/-(x - 1))
    else Num(x)

  def strings(t: Exp[(Int, String)]): String = t match {
    case Num(x) => x.toString
    case Mul((x, xs), (y, ys)) =>
      xs + " (" + x + ")" + ", " + ys + " (" + y + ")"
    case _ => Predef.???
  }

  def extract2and3(x: Int): Exp[Free[Exp, Int]] =
    // factors all the way down
    if (x > 2 && x % 2 == 0) Mul(Free.point(2), Free.point(x/2))
  // factors once and then stops
    else if (x > 3 && x % 3 == 0)
      Mul(Free.liftF(Num(3)), Free.liftF(Num(x/3)))
    else Num(x)

}
