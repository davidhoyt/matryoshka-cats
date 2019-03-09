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

trait BitraverseSyntax {
  import BitraverseSyntax._

  @inline implicit final def toCompatBitraverseInstanceOps[F[_, _]](b: Bitraverse[F]): BitraverseInstanceOps[F] = new BitraverseInstanceOps[F](b)
}

object BitraverseSyntax {
  final class BitraverseInstanceOps[F[_, _]](private val self: Bitraverse[F]) extends AnyVal {
    /** Extract the Traverse on the first param. */
  def leftTraverse[X]: Traverse[F[?, X]] =
    new LeftTraverse[F, X] {
      val F: Bitraverse[F] = self
    }

    /** Extract the Traverse on the second param. */
    def rightTraverse[X]: Traverse[F[X, ?]] =
      new RightTraverse[F, X] {
        val F: Bitraverse[F] = self
      }
  }
}
