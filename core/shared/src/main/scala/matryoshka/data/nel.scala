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
package data

import slamdata.Predef._

import compat._
import patterns._

import cats.data._

trait NonEmptyListInstances {
  @Deviation("Uses prepend instead of <:: and pattern matches uses List instead of IList.")
  implicit def nelBirecursive[A]
      : Birecursive.Aux[NonEmptyList[A], AndMaybe[A, ?]] =
    Birecursive.fromAlgebraIso({
      case Indeed(a, bs) => bs.prepend(a)
      case Only(a)       => NonEmptyList.one(a)
    },
      // TODO: restore exhaustivity when scalaz/scalaz#1363 is fixed, or we're on Cats.
      (_: NonEmptyList[A] @scala.unchecked) match {
        case NonEmptyList(a, b :: cs) => Indeed(a, NonEmptyList(b, cs))
        case NonEmptyList(a, Nil)     => Only(a)
      })
}

object nel extends NonEmptyListInstances
