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
import cats.free._

trait FreeSyntax {
  @inline implicit final def compatMonoidInstanceForTrampoline[B](implicit B: Monoid[B]): Monoid[Trampoline[B]] = FreeSyntax.compatMonoidInstanceForTrampoline
  @inline implicit final def toCompatFreeObjectOps(given: Free.type): FreeSyntax.FreeObjectOps = FreeSyntax.freeObjectOpsInstance
}

object FreeSyntax {
  private[FreeSyntax] val freeObjectOpsInstance = new FreeObjectOps(Free)

  final class FreeObjectOps(private val obj: Free.type) extends AnyVal {
    @inline def point[S[_], A](value: A): Free[S, A] = Free.pure[S, A](value)
  }

  implicit def compatMonoidInstanceForTrampoline[B](implicit B: Monoid[B]): Monoid[Trampoline[B]] =
    new Monoid[Trampoline[B]] {
      override val empty: Trampoline[B] = Trampoline.done(B.empty)

      override def combine(x: Trampoline[B], y: Trampoline[B]): Trampoline[B] =
        x.flatMap(x1 => y.map(y1 => B.combine(x1, y1)))
    }
}
