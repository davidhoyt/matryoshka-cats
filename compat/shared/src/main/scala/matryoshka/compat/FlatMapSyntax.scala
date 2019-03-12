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

trait FlatMapSyntax {
  import FlatMapSyntax._

  @inline implicit final def toCompatFlatMapOps[F[_], A](fa: F[A]): FlatMapOps[F, A] = new FlatMapOps[F, A](fa)

  @inline implicit final def toCompatFlatMapJoinOps[F[_], A](fa: F[F[A]]): FlatMapJoinOps[F, A] = new FlatMapJoinOps[F, A](fa)
}

object FlatMapSyntax {
  final class FlatMapOps[F[_], A](private val self: F[A]) extends AnyVal {
    //final def >>=[B](f: A => F[B])(implicit F: FlatMap[F]): F[B] = F.flatMap(self)(f)
  }

  final class FlatMapJoinOps[F[_], A](private val self: F[F[A]]) extends AnyVal {
    def join[B](implicit F: FlatMap[F]): F[A] = F.flatten(self)
  }
}
