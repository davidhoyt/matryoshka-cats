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
package patterns

import kernel._
import compat._

import cats._
import cats.free._
import cats.syntax.all._
import cats.instances.all._

/** The pattern functor for Free. */
final case class CoEnv[E, F[_], A](run: E \/ F[A])

object CoEnv extends CoEnvInstances {
  def coEnv[E, F[_], A](v: E \/ F[A]): CoEnv[E, F, A] = CoEnv(v)

  def hmap[F[_], G[_], A](f: F ~> G): CoEnv[A, F, ?] ~> CoEnv[A, G, ?] =
    λ[CoEnv[A, F, ?] ~> CoEnv[A, G, ?]](fa => CoEnv(fa.run.map(f(_))))

  def htraverse[G[_]: Applicative, F[_], H[_], A](f: F ~> (G ∘ H)#λ): CoEnv[A, F, ?] ~> (G ∘ CoEnv[A, H, ?])#λ =
    λ[CoEnv[A, F, ?] ~> (G ∘ CoEnv[A, H, ?])#λ](
      _.run.traverse(f(_)).map(CoEnv(_)))

  def freeIso[E, F[_]: Functor]: AlgebraIso[CoEnv[E, F, ?], Free[F, E]] =
    AlgebraIso[CoEnv[E, F, ?], Free[F, E]](
      coe => coe.run.fold(_.point[Free[F, ?]], Free.roll))(
      fr => CoEnv(fr.fold(_.left, _.right)))
}

sealed abstract class CoEnvInstances extends CoEnvInstances0 {
  implicit def equal[E: Equal, F[_]](implicit F: Delay[Equal, F]):
      Delay[Equal, CoEnv[E, F, ?]] =
    new Delay[Equal, CoEnv[E, F, ?]] {
      def apply[α](arb: Equal[α]) = {
        Equal.equal((a, b) => (a.run, b.run) match {
          case (-\/(e1), -\/(e2)) => e1 ≟ e2
          case (\/-(f1), \/-(f2)) => F(arb).equal(f1, f2)
          case (_,       _)       => false
        })
      }
    }

  implicit def show[E: Show, F[_]](implicit F: Delay[Show, F]): Delay[Show, CoEnv[E, F, ?]] =
    new Delay[Show, CoEnv[E, F, ?]] {
      def apply[A](sh: Show[A]) =
        Show.show(
          _.run.fold(
            e => Cord("-\\/(") ++ e.show,
            fa => Cord("\\/-(") ++ F(sh).show(fa)) ++
            Cord(")"))
    }

  // TODO: Need to have lower-prio instances of Bifoldable, with
  //       corresponding constraint on F.
  @Deviation("Defines bifoldRight and renames bitraverseImpl.")
  implicit def bitraverse[F[_]: Traverse]: Bitraverse[CoEnv[?, F, ?]] =
    new Bitraverse[CoEnv[?, F, ?]] {
      override def bifoldLeft[A, B, C](fab: CoEnv[A, F, B], c: C)(f: (C, A) => C, g: (C, B) => C): C =
        fab.run.bifoldLeft(c)(f, (c, fb) => fb.foldLeft(c)(g))

      override def bifoldRight[A, B, C](fab: CoEnv[A, F, B], c: Eval[C])(f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]): Eval[C] =
        fab.run.bifoldRight(c)(f, _.foldRight(_)(g))

      override def bitraverse[G[_]: Applicative, A, B, C, D](
        fab: CoEnv[A, F, B])(
        f: A ⇒ G[C], g: B ⇒ G[D]): G[CoEnv[C, F, D]] =
        fab.run.bitraverse(f, _.traverse(g)).map(CoEnv(_))
    }

  @Deviation("Uses explicit conversion to help the compiler.")
  implicit def traverse[F[_]: Traverse, E]: Traverse[CoEnv[E, F, ?]] =
    toCompatBitraverseInstanceOps[CoEnv[?, F, ?]](bitraverse[F]).rightTraverse

  // TODO: write a test to ensure the two monad instances are identical
  // implicit def monadCo[F[_]: Applicative: Comonad, A]: Monad[CoEnv[A, F, ?]] =
  //   new Monad[CoEnv[A, F, ?]] {
  //     def bind[B, C](fa: CoEnv[A, F, B])(f: (B) ⇒ CoEnv[A, F, C]) =
  //       CoEnv(fa.run >>= (fb => f(fb.copoint).run))
  //     def point[B](x: => B) = CoEnv(x.point[F].right)
  //   }
}

sealed abstract class CoEnvInstances0 {
  implicit def bifunctor[F[_]: Functor]: Bifunctor[CoEnv[?, F, ?]] =
    new Bifunctor[CoEnv[?, F, ?]] {
      def bimap[A, B, C, D](fab: CoEnv[A, F, B])(f: A ⇒ C, g: B ⇒ D) =
        CoEnv(fab.run.bimap(f, _.map(g)))
    }

  implicit def functor[F[_]: Functor, E]: Functor[CoEnv[E, F, ?]] =
    bifunctor[F].rightFunctor

  @Deviation("Defines bifoldLeft and bifoldRight using Eval.")
  implicit def bifoldable[F[_]: Foldable]: Bifoldable[CoEnv[?, F, ?]] =
    new Bifoldable[CoEnv[?, F, ?]] {
      override def bifoldMap[A, B, M: Monoid](fa: CoEnv[A, F, B])(f: A => M, g: B => M): M =
        fa.run.fold(f, _.foldMap(g))

      override def bifoldLeft[A, B, C](fa: CoEnv[A, F, B], z: C)(f: (C, A) => C, g: (C, B) => C): C =
        fa.run.fold(f(z, _), _.foldLeft(z)(g))

      override def bifoldRight[A, B, C](fa: CoEnv[A, F, B], z: Eval[C])(f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]): Eval[C] =
        fa.run.fold(f(_, z), _.foldRight(z)(g))
    }

  @Deviation("Uses explicit conversion to help the compiler.")
  implicit def foldable[F[_]: Foldable, E]: Foldable[CoEnv[E, F, ?]] =
    toCompatBifoldableInstanceOps[CoEnv[?, F, ?]](bifoldable[F]).rightFoldable

  // implicit def monad[F[_]: Monad: Traverse, A]: Monad[CoEnv[A, F, ?]] =
  //   new Monad[CoEnv[A, F, ?]] {
  //     def bind[B, C](fa: CoEnv[A, F, B])(f: (B) ⇒ CoEnv[A, F, C]) =
  //       CoEnv(fa.run >>= (_.traverse[CoEnv[A, F, ?], C](f).run.map(_.join)))
  //     def point[B](x: => B) = CoEnv(x.point[F].right)
  //   }
}
