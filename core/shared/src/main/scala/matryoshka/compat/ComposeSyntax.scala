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

import cats.arrow._

trait ComposeSyntax {
  import ComposeSyntax._

  @inline implicit final def toCompatComposeOps[F[_, _]: Compose, A, B](fab: F[A, B]): ComposeOps[F, A, B] = new ComposeOps[F, A, B](fab)
}

object ComposeSyntax {
  final class ComposeOps[F[_, _], A, B](val self: F[A, B])(implicit F: Compose[F]) extends Ops[F[A, B]] {
    @inline def ⋙[C](x: F[B, C]): F[A, C] = F.andThen(self, x)
  }
}
