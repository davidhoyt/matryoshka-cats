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
import cats.data._

trait OptionTSyntax {
  import OptionTSyntax._

  @inline implicit final def toCompatOptionTOps[F[_], A](instance: OptionT[F, A]): OptionTOps[F, A] = new OptionTOps[F, A](instance)
  @inline implicit final def toCompatOptionTObjectOps(given: OptionT.type): OptionTObjectOps.type = OptionTObjectOps
}

object OptionTSyntax {
  final class OptionTOps[F[_], A](private val instance: OptionT[F, A]) extends AnyVal {
    @inline def run: F[Option[A]] = instance.value
  }

  object OptionTObjectOps {
    @inline implicit def optionTFunctor[F[_]](implicit F0: Functor[F]): Functor[OptionT[F, ?]] =
      OptionT.catsDataFunctorForOptionT[F](F0)
  }
}
