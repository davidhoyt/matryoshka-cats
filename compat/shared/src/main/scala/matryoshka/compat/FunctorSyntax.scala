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

trait FunctorSyntax {
  import FunctorSyntax._

  @inline implicit final def toCompatFunctorOps[F[_], A](fa: F[A]): FunctorOps[F, A] = new FunctorOps[F, A](fa)
}

object FunctorSyntax {
  final class FunctorOps[F[_], A](private val fa: F[A]) extends AnyVal {
    def ∘[B](f: A => B)(implicit F: Functor[F]): F[B] = F.map(fa)(f)

    /** Inject `b` to the left of `A`s in `f`. */
    def strengthL[B](b: B)(implicit F: Functor[F]): F[(B, A)] = F.map(fa)(a => (b, a))
  }
}
