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

import cats.data._
import cats.implicits._

trait NonEmptyListSyntax {
  import NonEmptyListSyntax._

  @inline implicit final def toCompatNonEmptyListObjectOps(given: NonEmptyList.type): NonEmptyListObjectOps.type = NonEmptyListObjectOps
}

object NonEmptyListSyntax {
  object NonEmptyListObjectOps {
    @inline def nel[A](h: A, t: List[A]): NonEmptyList[A] = NonEmptyList(h, t)
    @inline def nonEmptyListEqual[A: Equal]: Equal[NonEmptyList[A]] = Equal.equalBy[NonEmptyList[A], List[A]](_.toList)
  }
}
