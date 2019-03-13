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

trait FoldableSyntax {
  import FoldableSyntax._

  @inline implicit final def toCompatFoldableOps[F[_], A](fa: F[A]): FoldableOps[F, A] = new FoldableOps[F, A](fa)
}

object FoldableSyntax {
  final class FoldableOps[F[_], A](private val self: F[A]) extends AnyVal {
    @inline def toStream(implicit F: Foldable[F]): Stream[A] =
      F.foldRight[A, Stream[A]](self, Eval.now(Stream.empty[A]))((a, evalXs) => evalXs.map(xs => Stream.cons(a, xs))).value

    @inline def toIList(implicit F: Foldable[F]): List[A] =
      F.toList(self)

    @inline def length(implicit F: Foldable[F]): Int =
      F.foldLeft(self, 0)((b, _) => b + 1)
  }
}
