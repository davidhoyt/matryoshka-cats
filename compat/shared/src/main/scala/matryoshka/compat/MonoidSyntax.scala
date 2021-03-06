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

trait MonoidSyntax {
  import MonoidSyntax._

  implicit def compatFunction1Monoid[B]: Monoid[B => B] =
    new Monoid[B => B] {
      override val empty: B => B = b => b
      override def combine(x: B => B, y: B => B): B => B = x compose y
    }

  @inline implicit final def toCompatMonoidInstanceOps[A](self: Monoid[A]): MonoidInstanceOps[A] = new MonoidInstanceOps[A](self)
}

object MonoidSyntax {
  final class MonoidInstanceOps[A](private val self: Monoid[A]) extends AnyVal {
    @inline def zero: A = self.empty
  }
}
