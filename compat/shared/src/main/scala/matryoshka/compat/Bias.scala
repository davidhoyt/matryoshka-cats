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
package compat

import slamdata.Predef._

import cats._

private trait LeftFunctor[F[_,_], X] extends Functor[F[?, X]] {
  implicit def F: Bifunctor[F]

  override def map[A, C](fax: F[A, X])(f: A => C): F[C, X] =
    F.bimap(fax)(f, x => x)
}

private trait RightFunctor[F[_,_], X] extends Functor[F[X, ?]] {
  implicit def F: Bifunctor[F]

  override def map[A, C](fax: F[X, A])(f: A => C): F[X, C] =
    F.bimap(fax)(x => x, f)
}

private trait UFunctor[F[_,_]] extends Functor[λ[α => F[α, α]]] {
  implicit def F: Bifunctor[F]

  override def map[A, C](fax: F[A, A])(f: A => C): F[C, C] =
    F.bimap(fax)(f, f)
}

private trait LeftFoldable[F[_,_], X] extends Foldable[F[?, X]] {
  implicit def F: Bifoldable[F]

  override def foldMap[A,B](fa: F[A, X])(f: A => B)(implicit B: Monoid[B]): B =
    F.bifoldMap(fa)(f, Function const B.empty)

  override def foldRight[A, B](fa: F[A, X], z: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    F.bifoldRight(fa, z)(f, (_, b) => b)

  override def foldLeft[A, B](fa: F[A, X], z: B)(f: (B, A) => B): B =
    F.bifoldLeft(fa, z)(f, (b, _) => b)
}

private trait RightFoldable[F[_,_], X] extends Foldable[F[X, ?]] {
  implicit def F: Bifoldable[F]

  override def foldMap[A,B](fa: F[X, A])(f: A => B)(implicit B: Monoid[B]): B =
    F.bifoldMap(fa)(Function const B.empty, f)

  override def foldRight[A, B](fa: F[X, A], z: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    F.bifoldRight(fa, z)((_, b) => b, f)

  override def foldLeft[A, B](fa: F[X, A], z: B)(f: (B, A) => B): B =
    F.bifoldLeft(fa, z)((b, _) => b, f)
}

private trait UFoldable[F[_,_]] extends Foldable[λ[α => F[α, α]]] {
  implicit def F: Bifoldable[F]

  override def foldMap[A,B](fa: F[A, A])(f: A => B)(implicit B: Monoid[B]): B =
    F.bifoldMap(fa)(f, f)

  override def foldRight[A, B](fa: F[A, A], z: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    F.bifoldRight(fa, z)(f, f)

  override def foldLeft[A, B](fa: F[A, A], z: B)(f: (B, A) => B): B =
    F.bifoldLeft(fa, z)(f, f)
}

private trait LeftTraverse[F[_,_], X] extends Traverse[F[?, X]]
    with LeftFunctor[F, X] with LeftFoldable[F, X] {
  implicit def F: Bitraverse[F]

  def traverse[G[_]:Applicative,A,B](fa: F[A, X])(f: A => G[B]): G[F[B, X]] =
    F.bitraverse(fa)(f, x => Applicative[G] point x)
}

private trait RightTraverse[F[_,_], X] extends Traverse[F[X, ?]]
    with RightFunctor[F, X] with RightFoldable[F, X] {
  implicit def F: Bitraverse[F]

  def traverse[G[_]:Applicative,A,B](fa: F[X, A])(f: A => G[B]): G[F[X, B]] =
    F.bitraverse(fa)(x => Applicative[G] point x, f)
}

private trait UTraverse[F[_,_]] extends Traverse[λ[α => F[α, α]]]
    with UFunctor[F] with UFoldable[F] {
  implicit def F: Bitraverse[F]

  def traverse[G[_]:Applicative,A,B](fa: F[A, A])(f: A => G[B]): G[F[B, B]] =
    F.bitraverse(fa)(f, f)
}
