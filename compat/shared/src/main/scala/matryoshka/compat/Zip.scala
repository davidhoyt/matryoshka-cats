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

trait Zip[F[_]] { self =>
  import Zip._

  ////
  def zip[A, B](a: Eval[F[A]], b: Eval[F[B]]): F[(A, B)]

  // derived functions

  /**The composition of Zip `F` and `G`, `[x]F[G[x]]`, is a Zip (if F is a Functor) */
  def compose[G[_]](implicit T0: Functor[F], G0: Zip[G]): Zip[λ[α => F[G[α]]]] =
    new CompositionZip[F, G] {
      implicit def T = T0
      implicit def F = self
      implicit def G = G0
    }

  /**The product of Zips `F` and `G`, `[x](F[x], G[x]])`, is a Zip */
  def product[G[_]](implicit G0: Zip[G]): Zip[λ[α => (F[α], G[α])]] =
    new ProductZip[F, G] {
      implicit def F = self
      implicit def G = G0
    }

  def zipWith[A, B, C](fa: Eval[F[A]], fb: Eval[F[B]])(f: (A, B) => C)(implicit F: Functor[F]): F[C] =
    F.map(zip(fa, fb)) {
      case (a, b) => f(a, b)
    }

  def apzip[A, B](f: Eval[F[A]] => Eval[F[B]], a: Eval[F[A]]): F[(A, B)] =
    zip(a, f(a))
}

object Zip {
  @inline def apply[F[_]](implicit F: Zip[F]): Zip[F] = F

  private trait CompositionZip[F[_], G[_]] extends Zip[λ[α => F[G[α]]]] {
    implicit def T: Functor[F]

    implicit def F: Zip[F]

    implicit def G: Zip[G]

    def zip[A, B](a: Eval[F[G[A]]], b: Eval[F[G[B]]]): F[G[(A, B)]] =
      F.zipWith(a, b)((a, b) => G.zip(Eval.now(a), Eval.now(b)))
  }

  private trait ProductZip[F[_], G[_]] extends Zip[λ[α => (F[α], G[α])]] {
    implicit def F: Zip[F]

    implicit def G: Zip[G]

    def zip[A, B](ea: Eval[(F[A], G[A])], eb: Eval[(F[B], G[B])]): (F[(A, B)], G[(A, B)]) =
      (F.zip(ea.map(_._1), eb.map(_._1)), G.zip(ea.map(_._2), eb.map(_._2)))
  }
}
