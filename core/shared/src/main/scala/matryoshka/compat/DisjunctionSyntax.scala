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

trait DisjunctionSyntax {
  import DisjunctionSyntax._

  type \/[+A, +B] = DisjunctionSyntax.\/[A, B]

  val \/ = DisjunctionSyntax.\/
  val -\/ = DisjunctionSyntax.-\/
  val \/- = DisjunctionSyntax.\/-

  @inline implicit final def toCompatDisjunctionValueOps[A](self: A): DisjunctionValueOps[A] = new DisjunctionValueOps[A](self)
  @inline implicit final def toCompatDisjunctionOps[A, B](given: A \/ B): DisjunctionOps[A, B] = new DisjunctionOps[A, B](given)
}

object DisjunctionSyntax {
  type \/[+A, +B] = Either[A, B]

  object \/ {
    implicit def DisjunctionInstances1[L]:  Traverse[L \/ ?] with MonadError[L \/ ?, L] = cats.instances.either.catsStdInstancesForEither[L]
  }

  object -\/ {
    @inline def apply[A](a: A): Either[A, Nothing] = Left(a)
    def unapply[A, B](arg: Either[A, B]): Option[A] = arg.fold(Some.apply, _ => None)
  }

  object \/- {
    @inline def apply[A](a: A): Either[Nothing, A] = Right(a)
    def unapply[A, B](arg: Either[A, B]): Option[B] = arg.fold(_ => None, Some.apply)
  }

  final class DisjunctionValueOps[A](private val self: A) extends AnyVal {
    @inline def left[B]: A \/ B = -\/(self)
    @inline def right[B]: B \/ A = \/-(self)
  }

  final class DisjunctionOps[A, B](private val self: A \/ B) extends AnyVal {
    @inline def bitraverse[F[_]: Functor, C, D](f: A => F[C], g: B => F[D]): F[C \/ D] =
      self match {
        case Left(a) => Functor[F].map(f(a))(c => Left[C, D](c))
        case Right(b) => Functor[F].map(g(b))(d => Right[C, D](d))
      }
  }
}
