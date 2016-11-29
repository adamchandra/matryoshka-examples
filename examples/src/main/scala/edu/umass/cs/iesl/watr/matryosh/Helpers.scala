package edu.umass.cs.iesl.watr
package matryosh

object ScalazTreeImplicits {
  import scalaz._
  import Scalaz._
  import textboxing.{TextBoxing => TB}
  import matryoshka._
  import matryoshka.implicits._

  // def boxT[
  //   T[_[_]] : RecursiveT: CorecursiveT,
  //   F[_]: Foldable: Functor](
  //   tf: T[F]
  // )(implicit
  //   FShow: Delay[Show, F]
  // ): TB.Box = {
  //   // tf.cata(toTree).drawBox
  //   tf.cataT(toTree).drawBox
  // }

  // implicit def cofreeRecursive[F[_], A]: Recursive.Aux[Cofree[F, A], EnvT[A, F, ?]] =
  def boxTF[T, F[_]: Foldable: Functor](
    tf: T
  )(implicit
    TR: Recursive.Aux[T, F],
    // TC: Corecursive.Aux[T, F],
    FShow: Delay[Show, F]
  ): TB.Box = {
    tf.cata(toTree).drawBox
  }

  implicit class RicherTree[A](val thisTree: scalaz.Tree[A]) extends AnyVal {

    def drawBox(implicit sh: Show[A]): TB.Box = {
      TB.linesToBox(thisTree.draw0)
    }

    def draw0(implicit sh: Show[A]): Stream[String] = {
      def drawSubTrees(s: List[Tree[A]]): Stream[String] = s match {
        case Nil      => Stream.Empty
        case t :: Nil => shift("╰─ ", "   ", t.draw0)
        case t :: ts  => shift("├─ ", "│  ", t.draw0) append drawSubTrees(ts)
      }
      def shift(first: String, other: String, s: Stream[String]): Stream[String] =
        (first #:: Stream.continually(other)).zip(s).map {
          case (a, b) => a + b
        }
      def mapParts[X, Y](as: Stream[X])(f: (X, Boolean, Boolean) => Y): Stream[Y] = {
        def loop(as: Stream[X], first: Boolean): Stream[Y] =
          if (as.isEmpty)           Stream.empty
          else if (as.tail.isEmpty) f(as.head, first, true) #:: Stream.empty
          else                      f(as.head, first, false) #:: loop(as.tail, false)
        loop(as, true)
      }

      val body = sh.shows(thisTree.rootLabel)
      val lines = body.split("\n").toStream
      mapParts(lines) { (a, first, last) =>
        a
      } ++ drawSubTrees(thisTree.subForest.toList)
    }

  }

}
