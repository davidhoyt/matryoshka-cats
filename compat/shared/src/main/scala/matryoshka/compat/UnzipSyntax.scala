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

trait UnzipSyntax {
  import UnzipSyntax._

  @inline implicit final def toCompatUnzipPairOps[F[_], A, B](self: F[(A, B)]): UnzipPairOps[F, A, B] = new UnzipPairOps[F, A, B](self)
}

object UnzipSyntax {
  final class UnzipPairOps[F[_], A, B](private val self: F[(A, B)]) extends AnyVal {
    def unfzip(implicit F: Unzip[F]): (F[A], F[B]) =
      F.unzip(self)
  }
}


