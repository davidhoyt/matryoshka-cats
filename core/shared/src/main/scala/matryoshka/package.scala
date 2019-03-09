/*
 * Copyright 2014–2018 SlamData Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import slamdata.Predef._

import cats._
import cats.free.{Free, Cofree, Yoneda}
import cats.implicits._

import cats.data.EitherT

import monocle._

import matryoshka.compat._
import matryoshka.data.free._
import matryoshka.implicits._
import matryoshka.patterns.EnvT

/** Generalized folds, unfolds, and refolds.
  *
  * @groupname algebras Algebras & Coalgebras
  * @groupdesc algebras Generic algebras that operate over most functors.
  * @groupname algtrans Algebra Transformations
  * @groupdesc algtrans Operations that modify algebras in various ways to make them easier to combine with others.
  * @groupname refolds Refolds
  * @groupdesc refolds Traversals that apply an unfold and a fold in a single pass.
  * @groupname dist Distributive Laws
  * @groupdesc dist Natural transformations required for generalized folds and unfolds.
  */
package object matryoshka {
  type ∘[F[_], G[_]] = matryoshka.Nested[F, G]

  /** Fold a structure `F` containing values in `W`, to a value `A`,
    * accumulating effects in the monad `M`.
    * @group algebras
    */
  type GAlgebraM[W[_], M[_], F[_], A] = F[W[A]] => M[A]

  /** Fold a structure `F` containing values in `W`, to a value `A`.
    * @group algebras
    */
  type GAlgebra[W[_], F[_], A]        = F[W[A]] => A    // GAlgebraM[W, Id, F, A]

  /** Fold a structure `F` to a value `A`, accumulating effects in the monad `M`.
    * @group algebras
    */
  type AlgebraM[M[_], F[_], A]        = F[A]    => M[A] // GAlgebraM[Id, M, F, A]

  /** Fold a structure `F` to a value `A`.
    * @group algebras
    */
  type Algebra[F[_], A]               = F[A] => A       // GAlgebra[Id, F, A]

  /** Fold a structure `F` (usually a `Functor`) contained in `W` (usually a
    * `Comonad`), to a value `A`, accumulating effects in the monad `M`.
    * @group algebras
    */
  type ElgotAlgebraM[W[_], M[_], F[_], A] =                        W[F[A]] => M[A]

  /** Fold a structure `F` (usually a `Functor`) contained in `W` (usually a
    * `Comonad`), to a value `A`.
    * @group algebras
    */
  type ElgotAlgebra[W[_], F[_], A] = ElgotAlgebraM[W, Id, F, A] // W[F[A]] => A

  /** Unfold a value `A` to a structure `F` containing values in `N`,
    * accumulating effects in the monad `M`.
    * @group algebras
    */
  type GCoalgebraM[N[_], M[_], F[_], A] = A => M[F[N[A]]]

  /** Unfold a value `A` to a structure `F` containing values in `N`.
    * @group algebras
    */
  type GCoalgebra[N[_], F[_], A]        = A => F[N[A]] // GCoalgebraM[N, Id, F, A]

  /** Unfold a value `A` to a structure `F`, accumulating effects in the monad
    * `M`.
    * @group algebras
    */
  type CoalgebraM[M[_], F[_], A]        = A => M[F[A]] // GCoalgebraM[Id, M, F, A]

  /** Unfold a value `A` to a structure `F`.
    * @group algebras
    */
  type Coalgebra[F[_], A]               = A => F[A]    // GCoalgebra[Id, F, A]

  /** Unfold a value `A` to a structure `F` (usually a `Functor`), contained in
    * `E`, accumulating effects in the monad `M`.
    * @group algebras
    */
  type ElgotCoalgebraM[E[_], M[_], F[_], A] = A => M[E[F[A]]]

  /** Unfold a value `A` to a structure `F` (usually a `Functor`), contained in
    * `E`.
    * @group algebras
    */
  type ElgotCoalgebra[E[_], F[_], A]        = A => E[F[A]] // ElgotCoalgebraM[E, Id, F, A]

  /** Transform a structure `F` containing values in `W`, to a structure `G`,
    * in bottom-up fashion.
    * @group algebras
    */
  type AlgebraicGTransformM[W[_], M[_], T, F[_], G[_]] = F[W[T]] => M[G[T]]

  /** Transform a structure `F` containing values in `W`, to a structure `G`,
    * in bottom-up fashion.
    * @group algebras
    */
  type AlgebraicGTransform[W[_], T, F[_], G[_]]        = F[W[T]] => G[T]

  /** Transform a structure `F` contained in `W`, to a structure `G`,
    * in bottom-up fashion.
    * @group algebras
    */
  type AlgebraicElgotTransformM[W[_], M[_], T, F[_], G[_]] = W[F[T]] => M[G[T]]

  /** Transform a structure `F` contained in `W`, to a structure `G`,
    * in bottom-up fashion.
    * @group algebras
    */
  type AlgebraicElgotTransform[W[_], T, F[_], G[_]] = W[F[T]] => G[T]

  /** Transform a structure `F` to a structure `G` containing values in `N`,
    * in top-down fashion, accumulating effects in the monad `M`.
    * @group algebras
    */
  type CoalgebraicGTransformM[N[_], M[_], T, F[_], G[_]] = F[T] => M[G[N[T]]]

  /** Transform a structure `F` to a structure `G`, in top-down fashion,
    * accumulating effects in the monad `M`.
    * @group algebras
    */
  type CoalgebraicGTransform[N[_], T, F[_], G[_]]        = F[T] => G[N[T]]

  /** Transform a structure `F` to a structure `G`, in top-down fashion,
    * accumulating effects in the monad `M`.
    * @group algebras
    */
  type CoalgebraicElgotTransform[N[_], T, F[_], G[_]]    = F[T] => N[G[T]]

  /** Transform a structure `F` to a structure `G`, accumulating effects in the
    *  monad `M`. For a top-down transformation, `T#Base` must be `F`, and for a
    *  bottom-up transformation, `T#Base` must be `G`.
    *
    * @group algebras
    */
  type TransformM[M[_], T, F[_], G[_]]        = F[T] => M[G[T]]

  /** Transform a structure `F` to a structure `G`. For a top-down
    * transformation, `T#Base` must be `F`, and for a bottom-up transformation,
    * `T#Base` must be `G`.
    *
    * @group algebras
    */
  type Transform[T, F[_], G[_]]               = F[T] => G[T] // TransformM[Id, T, F, G]

  type EndoTransform[T, F[_]]                 = F[T] => F[T] // Transform[T, F, F]

  /** An algebra and its dual form an isomorphism.
    */
  type GAlgebraIso[W[_], N[_], F[_], A] = PIso[F[W[A]], F[N[A]], A, A]
  object GAlgebraIso {
    def apply[W[_], N[_], F[_], A](φ: GAlgebra[W, F, A])(ψ: GCoalgebra[N, F, A])
        : GAlgebraIso[W, N, F, A] =
      PIso(φ)(ψ)
  }

  type ElgotAlgebraIso[W[_], N[_], F[_], A] = PIso[W[F[A]], N[F[A]], A, A]
  object ElgotAlgebraIso {
    def apply[W[_], N[_], F[_], A](φ: ElgotAlgebra[W, F, A])(ψ: ElgotCoalgebra[N, F, A])
        : ElgotAlgebraIso[W, N, F, A] =
      PIso(φ)(ψ)
  }

  type AlgebraIso[F[_], A] = GAlgebraIso[Id, Id, F, A]
  object AlgebraIso {
    def apply[F[_], A](φ: Algebra[F, A])(ψ: Coalgebra[F, A]):
        AlgebraIso[F, A] =
      Iso(φ)(ψ)
  }

  type AlgebraPrism[F[_], A] = Prism[F[A], A]
  object AlgebraPrism {
    def apply[F[_], A](φ: AlgebraM[Option, F, A])(ψ: Coalgebra[F, A]):
        AlgebraPrism[F, A] =
      Prism(φ)(ψ)
  }

  type CoalgebraPrism[F[_], A] = Prism[A, F[A]]
  object CoalgebraPrism {
    def apply[F[_], A](ψ: CoalgebraM[Option, F, A])(φ: Algebra[F, A]):
        CoalgebraPrism[F, A] =
      Prism(ψ)(φ)
  }

  /** There is a fold/unfold isomorphism for any AlgebraIso.
    */
  def foldIso[T, F[_]: Functor, A]
    (alg: AlgebraIso[F, A])
    (implicit T: Birecursive.Aux[T, F]): Iso[T, A] =
    Iso[T, A](_.cata(alg.get))(_.ana[T](alg.reverseGet))

  /** There is a fold prism for any AlgebraPrism.
    */
  def foldPrism[T, F[_]: Traverse, A]
    (alg: AlgebraPrism[F, A])
    (implicit T: Birecursive.Aux[T, F]): Prism[T, A] =
    Prism[T, A](T.cataM(_)(alg.getOption))(_.ana[T](alg.reverseGet))

  /** There is an unfold prism for any CoalgebraPrism.
    */
  def unfoldPrism[T, F[_]: Traverse, A]
    (coalg: CoalgebraPrism[F, A])
    (implicit T: Birecursive.Aux[T, F]): Prism[A, T] =
    Prism[A, T](_.anaM[T](coalg.getOption))(_.cata(coalg.reverseGet))

  /** A NaturalTransformation that sequences two types.
    *
    * `F[G[_]] ~> G[F[_]]`
    *
    * @group dist
    */
  type DistributiveLaw[F[_], G[_]] = (F ∘ G)#λ ~> (G ∘ F)#λ

  /** The hylomorphism is the fundamental operation of recursion schemes. It
    * first applies `ψ`, recursively breaking an `A` into layers of `F`, then
    * applies `φ`, combining the `F`s into a `B`. It can also be seen as the
    * (fused) composition of an anamorphism and a catamorphism that avoids
    * building the intermediate recursive data structure.
    *
    * @group refolds
    */
  @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
  @inline def hylo[F[_]: Functor, A, B](a: A)(φ: Algebra[F, B], ψ: Coalgebra[F, A]): B =
    φ(ψ(a) ∘ (hylo(_)(φ, ψ)))

  /** A Kleisli hylomorphism.
    *
    * @group refolds
    */
  def hyloM[M[_], F[_], A, B]
    (a: A)
    (f: AlgebraM[M, F, B], g: CoalgebraM[M, F, A])
    (implicit M: Monad[M], F: Traverse[F])
      : M[B] =
    hylo[(M ∘ F)#λ, A, M[B]](a)(_ >>= (_.sequence >>= f), g)(M compose F)

    /** Composition of an elgot-anamorphism and an elgot-catamorphism that avoids
    * building the intermediate recursive data structure.
    *
    * `elgotCata ⋘ elgotAna`
    *
    * @group refolds
    */
  def elgotHylo[W[_]: Comonad, N[_]: Monad, F[_]: Functor, A, B]
      (a: A)
      (kφ: DistributiveLaw[F, W], kψ: DistributiveLaw[N, F], φ: ElgotAlgebra[W, F, B], ψ: ElgotCoalgebra[N, F, A])
        : B = {
    lazy val trans: N[A] => W[B] =
      na => loop(na >>= ψ) cobind φ
    lazy val loop: N[F[A]] => W[F[B]] =
      (kψ[A] _) ⋙ (_ map trans) ⋙ kφ[B]
    (ψ ⋙ loop ⋙ φ)(a)
  }

  /** `histo ⋘ ana`
    *
    * @group refolds
    */
  def dyna[F[_]: Functor, A, B](a: A)(φ: F[Cofree[F, B]] => B, ψ: Coalgebra[F, A]): B =
    ghylo[Cofree[F, ?], Id, F, A, B](a)(distHisto, distAna, φ, ψ)

  /** `cata ⋘ futu`
    *
    * @group refolds
    */
  def codyna[F[_]: Functor, A, B](a: A)(φ: Algebra[F, B], ψ: GCoalgebra[Free[F, ?], F, A]): B =
    ghylo[Id, Free[F, ?], F, A, B](a)(distCata, distFutu, φ, ψ)

  /** `cataM ⋘ futuM`
    *
    * @group refolds
    */
  def codynaM[M[_]: Monad, F[_]: Traverse, A, B](a: A)(φ: AlgebraM[M, F, B], ψ: GCoalgebraM[Free[F, ?], M, F, A]): M[B] =
    ghyloM[Id, Free[F, ?], M, F, A, B](a)(distCata, distFutu, φ, ψ)

  /**
    *
    * @group dist
    */
  def distHisto[F[_]: Functor]: DistributiveLaw[F, Cofree[F, ?]] =
    new DistributiveLaw[F, Cofree[F, ?]] {
      def apply[α](m: F[Cofree[F, α]]): Cofree[F, F[α]] =
        distGHisto[F, F](NaturalTransformation.refl[λ[α => F[F[α]]]]).apply(m)
    }

  /**
    *
    * @group dist
    */
  // TODO: Should be able to generalize this over `Recursive.Aux[T, EnvT[…]]`
  //       somehow, then it wouldn’t depend on Scalaz and would work with any
  //       `Cofree` representation.
  @DeviatesFromScalaZ
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def distGHisto[F[_]: Functor,  H[_]: Functor](k: DistributiveLaw[F, H]): DistributiveLaw[F, Cofree[H, ?]] =
    new DistributiveLaw[F, Cofree[H, ?]] {
      private[this] type FnEvalWithK[A] = FEval[F, H, Cofree[H, A]] => EvalK[H, F, Cofree[H, A]]
      private[this] val evalWithKAny: FnEvalWithK[Any] = safeDistEvalN[F, H, Cofree[H, Any]](k)
      private[this] def evalWithK[A]: FnEvalWithK[A] = evalWithKAny.asInstanceOf[FnEvalWithK[A]]

      def apply[α](m: F[Cofree[H, α]]): Cofree[H, F[α]] =
        Cofree.compatUnfoldLazy(m) { as =>
          val head: F[α] = as ∘ (_.copure)
          val tailStep1: F[Eval[H[Cofree[H, α]]]] = as ∘ (_.tail)
          val tailStep2: Eval[H[F[Cofree[H, α]]]] = evalWithK(tailStep1)
          (head, tailStep2)
        }
    }

  /**
    *
    * @group dist
    */
  def distAna[F[_]]: DistributiveLaw[Id, F] = NaturalTransformation.refl

  /**
    *
    * @group dist
    */
  @DeviatesFromScalaZ
  def distApo[T, F[_]: Functor](implicit T: Recursive.Aux[T, F])
      : DistributiveLaw[T \/ ?, F] =
    distGApo[F, T](T.project(_))

  /**
    *
    * @group dist
    */
  @DeviatesFromScalaZ
  def distGApo[F[_] : Functor, B](g: Coalgebra[F, B]): DistributiveLaw[B \/ ?, F] =
    new DistributiveLaw[B \/ ?, F] {
      def apply[α](m: B \/ F[α]): F[B \/ α] = m.bitraverse(g(_), x => x)
    }

  /** Allows for more complex unfolds, like
    * `futuGApo(φ0: Coalgebra[F, B], φ: GCoalgebra[λ[α => EitherT[Free[F, ?], B, α]], F, A])`
    *
    * @group dist
    */
  @DeviatesFromScalaZ
  def distGApoT[F[_]: Functor, M[_]: Functor, B](
    g: Coalgebra[F, B], k: DistributiveLaw[M, F]): DistributiveLaw[EitherT[M, B, ?], F] =
    new DistributiveLaw[EitherT[M, B, ?], F] {
      def apply[α](m: EitherT[M, B, F[α]]): F[EitherT[M, B, α]] =
        k(m.run.map(distGApo(g).apply(_))).map(EitherT(_))
    }

  /** A generalized version of a hylomorphism that composes any coalgebra and
    * algebra. (`gcata ⋘ gana`)
    *
    * @group refolds
    */
  def ghylo[W[_]: Comonad, N[_]: Monad, F[_]: Functor, A, B](
    a: A)(
    w: DistributiveLaw[F, W],
    n: DistributiveLaw[N, F],
    f: GAlgebra[W, F, B],
    g: GCoalgebra[N, F, A]):
      B =
    hylo[Yoneda[F, ?], N[A], W[B]](
      a.point[N])(
      fwb => w((fwb ∘ (_.cojoin)).run) ∘ f, na => Yoneda(n(na ∘ g)) ∘ (_.join)).copoint

  /** A Kleisli `ghylo` (`gcataM ⋘ ganaM`)
    *
    * @group refolds
    */
  def ghyloM[W[_]: Comonad: Traverse, N[_]: Monad: Traverse, M[_]: Monad, F[_]: Traverse, A, B](
    a: A)(
    w: DistributiveLaw[F, W],
    n: DistributiveLaw[N, F],
    f: GAlgebraM[W, M, F, B],
    g: GCoalgebraM[N, M, F, A]):
      M[B] =
    hyloM[M, F, N[A], W[B]](
      a.point[N])(
      fwb => w(fwb ∘ (_.cojoin)).traverse(f),
        na => na.traverse(g) ∘ (n(_) ∘ (_.join))) ∘
      (_.copoint)

  /** Similar to a hylomorphism, this composes a futumorphism and a
    * histomorphism.
    *
    * @group refolds
    */
  def chrono[F[_]: Functor, A, B](
    a: A)(
    g: GAlgebra[Cofree[F, ?], F, B], f: GCoalgebra[Free[F, ?], F, A]):
      B =
    ghylo[Cofree[F, ?], Free[F, ?], F, A, B](a)(distHisto, distFutu, g, f)

  /** `cata ⋘ elgotGApo`
    *
    * @group refolds
    */
  def elgot[F[_], A, B]
    (a: A)
    (φ: Algebra[F, B], ψ: ElgotCoalgebra[B \/ ?, F, A])
    (implicit F: Functor[F])
      : B =
    hylo[((B \/ ?) ∘ F)#λ, A, B](a)(_.swap.valueOr(φ), ψ)(Functor[B \/ ?] compose F)

  /** `cataM ⋘ elgotGApoM`
    *
    * @group refolds
    */
  def elgotM[M[_], F[_], A, B]
    (a: A)
    (φ: AlgebraM[M, F, B], ψ: ElgotCoalgebraM[B \/ ?, M, F, A])
    (implicit M: Monad[M], F: Traverse[F])
      : M[B] =
    hyloM[M, ((B \/ ?) ∘ F)#λ, A, B](
      a)(
      _.fold(_.point[M], φ), ψ)(
      M, Traverse[B \/ ?] compose F)

  /** `elgotZygo ⋘ ana`
    *
    * @group refolds
    */
  def coelgot[F[_], A, B]
    (a: A)
    (φ: ElgotAlgebra[(A, ?), F, B], ψ: Coalgebra[F, A])
    (implicit F: Functor[F])
      : B =
    hylo[((A, ?) ∘ F)#λ, A, B](a)(φ, a => (a, ψ(a)))(Functor[(A, ?)] compose F)

  /** `elgotZygoM ⋘ anaM`
    *
    * @group refolds
    */
  object coelgotM {
    def apply[M[_]]: PartiallyApplied[M] = new PartiallyApplied[M]

    final class PartiallyApplied[M[_]] {
      def apply[F[_], A, B]
        (a: A)
        (φ: ElgotAlgebraM[(A, ?), M, F, B], ψ: CoalgebraM[M, F, A])
        (implicit M: Monad[M], F: Traverse[F])
          : M[B] =
        hyloM[M, ((A, ?) ∘ F)#λ, A, B](
          a)(
          φ, a => ψ(a) strengthL a)(
          M, Traverse[(A, ?)] compose F)
    }
  }

//  def transHylo[T, F[_]: Functor, G[_]: Functor, U, H[_]: Functor]
//    (t: T)
//    (φ: G[U] => H[U], ψ: F[T] => G[T])
//    (implicit T: Recursive.Aux[T, F], U: Corecursive.Aux[U, H])
//      : U =
//    hylo(t)(φ >>> (U.embed(_)), ψ <<< (T.project(_)))

//  /**
//    *
//    * @group dist
//    */
//  def distPara[T, F[_]: Functor](implicit T: Corecursive.Aux[T, F])
//      : DistributiveLaw[F, (T, ?)] =
//    distZygo[F, T](T.embed(_))

  /**
    *
    * @group dist
    */
  def distFutu[F[_]: Functor]: DistributiveLaw[Free[F, ?], F] =
    new DistributiveLaw[Free[F, ?], F] {
      def apply[α](m: Free[F, F[α]]): F[Free[F, α]] =
        distGFutu[F, F](NaturalTransformation.refl[λ[α => F[F[α]]]]).apply(m)
    }

  /**
    *
    * @group dist
    */
  // TODO: Should be able to generalize this over `Recursive.Aux[T, CoEnv[…]]`
  //       somehow, then it wouldn’t depend on Scalaz and would work with any
  //       `Free` representation.
  def distGFutu[H[_]: Functor, F[_]: Functor](k: DistributiveLaw[H, F])
      : DistributiveLaw[Free[H, ?], F] =
    new DistributiveLaw[Free[H, ?], F] {
      def apply[A](m: Free[H, F[A]]): F[Free[H, A]] =
        m.cata[F[Free[H, A]]](_.run.fold(_ ∘ Free.point, k(_) ∘ Free.roll))
    }

  def transHylo[T, F[_]: Functor, G[_]: Functor, U, H[_]: Functor]
    (t: T)
    (φ: G[U] => H[U], ψ: F[T] => G[T])
    (implicit T: Recursive.Aux[T, F], U: Corecursive.Aux[U, H])
      : U =
    hylo(t)(φ >>> (U.embed(_)), ψ <<< (T.project(_)))

  /**
    *
    * @group dist
    */
  def distPara[T, F[_]: Functor](implicit T: Corecursive.Aux[T, F])
      : DistributiveLaw[F, (T, ?)] =
    distZygo[F, T](T.embed(_))

  /**
    *
    * @group dist
    */
  @DeviatesFromScalaZ
  def distParaT[T, W[_]: Comonad, F[_]: Functor]
    (t: DistributiveLaw[F, W])
    (implicit T: Corecursive.Aux[T, F])
      : DistributiveLaw[F, EnvT[T, W, ?]] =
    distZygoT[F, W, T](T.embed(_), t)

  /**
    *
    * @group dist
    */
  def distCata[F[_]]: DistributiveLaw[F, Id] = NaturalTransformation.refl

  /** A general [[DistributiveLaw]] for the case where the [[cats.Comonad]] is
    * also [[cats.Applicative]].
    *
    * @group dist
    */
  def distApplicative[F[_]: Traverse, G[_]: Applicative]: (F ∘ G)#λ ~> (G ∘ F)#λ =
    new DistributiveLaw[F, G] {
      def apply[A](fga: F[G[A]]): G[F[A]] = fga.sequence
    }

  /**
    *
    * @group dist
    */
  def distZygo[F[_]: Functor, B](g: Algebra[F, B]): DistributiveLaw[F, (B, ?)] =
    new DistributiveLaw[F, (B, ?)] {
      def apply[α](m: F[(B, α)]): (B, F[α]) = (g(m ∘ (_._1)), m ∘ (_._2))
    }

    /**
    *
    * @group dist
    */
  def distZygoM[F[_]: Functor, M[_]: Monad, B](
    g: AlgebraM[M, F, B], k: DistributiveLaw[F, M]): DistributiveLaw[F, (M ∘ (B, ?))#λ] =
    new DistributiveLaw[F, (M ∘ (B, ?))#λ] {
      def apply[α](fm: F[M[(B, α)]]): M[(B, F[α])] = {
        k(fm) >>= { f => g(f ∘ (_._1)) ∘ ((_, f ∘ (_._2))) }
      }
    }

  /**
    *
    * @group dist
    */
  def distZygoT[F[_]: Functor, W[_]: Comonad, B](
    g: Algebra[F, B], k: DistributiveLaw[F, W]): DistributiveLaw[F, EnvT[B, W, ?]] =
    new DistributiveLaw[F, EnvT[B, W, ?]] {
      def apply[α](fe: F[EnvT[B, W, α]]): EnvT[B, W, F[α]] =
        EnvT((g(fe ∘ (_.ask)), k(fe ∘ (_.lower))))
    }

  def holes[F[_]: Traverse, A](fa: F[A]): F[(A, Coalgebra[F, A])] =
    (fa.mapAccumL(0) {
      case (i, x) =>
        val h: Coalgebra[F, A] = { y =>
          val g: (Int, A) => (Int, A) = (j, z) => (j + 1, if (i == j) y else z)

          fa.mapAccumL(0)(g)._2
        }

        (i + 1, (x, h))
    })._2

  def holesList[F[_]: Traverse, A](fa: F[A]): List[(A, Coalgebra[F, A])] =
    holes(fa).toList

  def builder[F[_]: Traverse, A, B](fa: F[A], children: List[B]): F[B] = {
    (fa.mapAccumL(children) {
      case (x :: xs, _) => (xs, x)
      case _ => scala.sys.error("Not enough children")
    })._2
  }

  def project[F[_]: Foldable, A](index: Int, fa: F[A]): Option[A] =
   if (index < 0) None
   else fa.toList.drop(index).headOption
}
