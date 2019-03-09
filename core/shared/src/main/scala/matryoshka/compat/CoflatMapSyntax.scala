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

trait CoflatMapSyntax {
  import CoflatMapSyntax._

  @inline implicit final def toCompatCoflatMapOps[F[_], A](fa: F[A]): CoflatMapOps[F, A] = new CoflatMapOps[F, A](fa)
  @inline implicit final def toCompatComonadWithCobind[F[_]](given: Comonad[F]): Comonad[F] with Cobind[F] = withCobind(given)
}

object CoflatMapSyntax {
  final class CoflatMapOps[F[_], A](private val fa: F[A]) extends AnyVal {
    @inline def cobind[B](f: F[A] => B)(implicit F: CoflatMap[F]): F[B] = F.coflatMap(fa)(f)
    @inline def cojoin(implicit F: CoflatMap[F]): F[F[A]] = F.coflatten(fa)
  }

  private class DelegatingComonad[F[_]](private val given: Comonad[F]) extends Comonad[F] with Cobind[F] {
    override def extract[A](x: F[A]): A = given.extract(x)
    override def map[A, B](fa: F[A])(f: A => B): F[B] = given.map(fa)(f)
    override def cobind[A, B](fa: F[A])(f: F[A] => B): F[B] = given.coflatMap(fa)(f)
  }

  def withCobind[F[_]](given: Comonad[F]): Comonad[F] with Cobind[F] =
    new DelegatingComonad[F](given)
}



trait Cobind[F[_]] extends CoflatMap[F] {
  def cobind[A, B](fa: F[A])(f: F[A] => B): F[B]
  def cojoin[A](fa: F[A]): F[F[A]] = cobind(fa)(fa => fa)
  final override def coflatMap[A, B](fa: F[A])(f: F[A] => B): F[B] = cobind(fa)(f)
}
