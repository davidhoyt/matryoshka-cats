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

package matryoshka

import cats._

import monocle._

package object kernel {
  type ∘[F[_], G[_]] = Nested[F, G]

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

  type ElgotAlgebraIso[W[_], N[_], F[_], A] = PIso[W[F[A]], N[F[A]], A, A]

  type AlgebraIso[F[_], A] = GAlgebraIso[Id, Id, F, A]

  type AlgebraPrism[F[_], A] = Prism[F[A], A]

  type CoalgebraPrism[F[_], A] = Prism[A, F[A]]

  /** A NaturalTransformation that sequences two types.
    *
    * `F[G[_]] ~> G[F[_]]`
    *
    * @group dist
    */
  type DistributiveLaw[F[_], G[_]] = (F ∘ G)#λ ~> (G ∘ F)#λ
}
