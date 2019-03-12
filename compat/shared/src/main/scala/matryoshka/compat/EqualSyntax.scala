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

import cats.kernel.Eq
import cats.instances.unit._

trait EqualSyntax {
  import EqualSyntax._

  type Equal[A] = EqualSyntax.Equal[A]

  implicit val equalUnit: Equal[Unit] = EqualSyntax.Equal.unit

  val Equal: EqualSyntax.Equal.type = EqualSyntax.Equal

  @inline implicit final def toCompatEqualOps[A](a: A): EqualOps[A] = new EqualOps[A](a)
  @inline implicit final def toCompatEqualInstanceOps[F](e: Eq[F]): EqualInstanceOps[F] = new EqualInstanceOps[F](e)
}

@SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf", "org.wartremover.warts.Equals"))
object EqualSyntax {
  type Equal[A] = Eq[A]

  final class EqualOps[A](private val self: A) extends AnyVal {
    def ≟(other: A)(implicit E: Eq[A]): Boolean = (self.asInstanceOf[AnyRef] eq other.asInstanceOf[AnyRef]) || E.eqv(self, other)
    @inline def equal(other: A)(implicit E: Eq[A]): Boolean = E.eqv(self, other)
  }

  final class EqualInstanceOps[F](private val self: Eq[F]) extends AnyVal {
    @inline def equal(a1: F, a2: F): Boolean = self.eqv(a1, a2)
  }

  object Equal {
    val unit: Equal[Unit] = Eq[Unit]

    @inline def apply[A](implicit e: Equal[A]): Equal[A] = e
    @inline def equal[A](f: (A, A) => Boolean): Equal[A] = Eq.instance(f)
    @inline def equalBy[A, B](f: A => B)(implicit B: Equal[B]): Equal[A] = (a1: A, a2: A) => B.eqv(f(a1), f(a2))
  }
}
