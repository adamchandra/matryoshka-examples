package edu.umass.cs.iesl.watr
package experiment


import scalaz.~>
import scalaz.Functor
import scala.reflect._
import matryoshka._

trait NamedTransforms {

  // import sourcecode._
  import sourcecode.Text

  def namedDistLaw[F[_]:Functor, G[_]](
    dl: DistributiveLaw[F, G]
  )(implicit
    fct: ClassTag[F[_]],
    gct: ClassTag[G[_]]
  ): DistributiveLaw[F, G] = new DistributiveLaw[F, G] {
    def apply[A](fa: F[G[A]]) = {
      dl.apply(fa)
    }
    override def toString() = {
      val fname = ""// dl.source
      val fn = fct.runtimeClass.getSimpleName
      val gn = gct.runtimeClass.getSimpleName
      s"<$fname:$fn[$gn]~>$gn[$fn]>"
    }
  }


  def namedNatTrans[F[_], G[_]](
    nt: Text[F ~> G]
  )(implicit
    fct: ClassTag[F[_]],
    gct: ClassTag[G[_]]
  ): F ~> G = new (F ~> G) {
    def apply[A](fa: F[A]) = {
      nt.value.apply(fa)
    }
    override def toString() = {
      val fname = nt.source
      val fn = fct.runtimeClass.getSimpleName
      val gn = gct.runtimeClass.getSimpleName
      s"<$fname:$fn~>$gn>"
    }
  }

  // type AlgebraicTransform[T[_[_]], F[_], G[_]] = F[T[G]] => G[T[G]]
  def namedAlgebraicTrans[T[_[_]], F[_], G[_]](
    alg: Text[AlgebraicTransform[T, F, G]]
  )(implicit
    // tct: ClassTag[T[_[_]]],
    fct: ClassTag[F[_]],
    gct: ClassTag[G[_]]
  ): AlgebraicTransform[T, F, G] = new AlgebraicTransform[T, F, G] {
    def apply(ftg: F[T[G]]) = {
      alg.value.apply(ftg)
    }
    override def toString() = {
      val fname = alg.source
      val fn = fct.runtimeClass.getSimpleName
      val gn = gct.runtimeClass.getSimpleName
      s"<$fname:$fn[T[$gn]]=>$gn[T[$gn]]>"
    }
  }
  def namedAlgebra[F[_], A](
    alg: Text[Algebra[F, A]]
  )(implicit
    fct: ClassTag[F[_]],
    act: ClassTag[A]
  ): Algebra[F, A] = new Algebra[F, A] {
    def apply(fa: F[A]) = {
      alg.value.apply(fa)
    }
    override def toString() = {
      val fname = alg.source
      val fn = fct.runtimeClass.getSimpleName
      val an = act.runtimeClass.getSimpleName
      s"<$fname:$fn[$an]=>$an>"
    }
  }


  // type GAlgebra[W[_], F[_], A] = F[W[A]] => A
  def namedGAlgebra[W[_], F[_], A](
    // alg: GAlgebra[W, F, A]
      alg: Text[GAlgebra[W, F, A]]
  )(implicit
    wct: ClassTag[W[_]],
    fct: ClassTag[F[_]],
    act: ClassTag[A]
  ): GAlgebra[W, F, A] = new GAlgebra[W, F, A] {
    def apply(fwa: F[W[A]]) = {
      alg.value.apply(fwa)
    }
    override def toString() = {
      val fname = alg.source
      val fn = fct.runtimeClass.getSimpleName
      val wn = wct.runtimeClass.getSimpleName
      val an = act.runtimeClass.getSimpleName
      s"<$fname:$fn[$wn[$an]]=>$an>"
    }
  }
}

object NamedTransforms extends NamedTransforms
