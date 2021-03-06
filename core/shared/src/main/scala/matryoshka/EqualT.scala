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

import kernel._
import compat._
import implicits._

import slamdata.Predef._

import cats._
import simulacrum._

@typeclass trait EqualT[T[_[_]]] {
  def equal[F[_]: Functor](tf1: T[F], tf2: T[F])(implicit del: Delay[Equal, F]):
      Boolean

  def equalT[F[_]: Functor](delay: Delay[Equal, F]): Equal[T[F]] =
    Equal.equal[T[F]](equal[F](_, _)(Functor[F], delay))
}

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object EqualT {
  def recursiveT[T[_[_]]: RecursiveT]: EqualT[T] = new EqualT[T] {
    def equal[F[_]: Functor]
      (tf1: T[F], tf2: T[F])
      (implicit del: Delay[Equal, F]): Boolean =
      del(equalT[F](del)).equal(tf1.project, tf2.project)
  }
}
