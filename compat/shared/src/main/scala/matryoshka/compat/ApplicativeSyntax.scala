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

trait ApplicativeSyntax {
  import ApplicativeSyntax._

  @inline implicit final def toCompatApplicativeIdOps[A](a: A): ApplicativeIdOps[A] = new ApplicativeIdOps[A](a)
  @inline implicit final def toCompatApplicativeInstanceOps[F[_]](instance: Applicative[F]): ApplicativeInstanceOps[F] = new ApplicativeInstanceOps[F](instance)
}

object ApplicativeSyntax {
  final class ApplicativeIdOps[A](private val a: A) extends AnyVal {
    def point[F[_]](implicit A: Applicative[F]): F[A] = A.pure(a)
  }

  final class ApplicativeInstanceOps[F[_]](private val instance: Applicative[F]) extends AnyVal {
    def apply2[A, B, C](fa: => F[A], fb: => F[B])(f: (A, B) => C): F[C] =
      instance.map2(fa, fb)(f)
  }
}
