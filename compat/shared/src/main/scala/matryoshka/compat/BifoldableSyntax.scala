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

trait BifoldableSyntax {
  import BifoldableSyntax._

  @inline implicit final def toCompatBifoldableInstanceOps[F[_, _]](b: Bifoldable[F]): BifoldableInstanceOps[F] = new BifoldableInstanceOps[F](b)
}

object BifoldableSyntax {
  final class BifoldableInstanceOps[F[_, _]](private val self: Bifoldable[F]) extends AnyVal {
    /** Extract the Foldable on the second parameter. */
    def rightFoldable[X]: Foldable[F[X, ?]] =
      new RightFoldable[F, X] {
        val F: Bifoldable[F] = self
      }
  }
}
