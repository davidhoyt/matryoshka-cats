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

import cats.Show

trait ShowSyntax {
  import ShowSyntax._

  type Cord = String

  type CordString = ShowSyntax.CordString

  val Cord: ShowSyntax.Cord.type = ShowSyntax.Cord

  @inline implicit final def toCompatShowOps[F](f: F): ShowOps[F] = new ShowOps[F](f)

  @inline implicit final def toCompatShowInstanceOps[F](s: Show[F]): ShowInstanceOps[F] = new ShowInstanceOps[F](s)

  @inline implicit final def toCompatCordStringOps(s: CordString): CordStringOps = new CordStringOps(s)
}

object ShowSyntax {
  type CordString = String with Cord

  sealed trait Cord

  object Cord {
    @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
    @inline final def apply(given: String): CordString = given.asInstanceOf[CordString]

    implicit final val CordShow: Show[String] = Show.show[String](x => x)
  }

  final class ShowOps[F](private val self: F) extends AnyVal {
    //def show(implicit F: Show[F]): String = F.show(self)
  }

  final class ShowInstanceOps[F](private val self: Show[F]) extends AnyVal {
    def shows(given: F): String = self.show(given)
  }

  final class CordStringOps(private val self: CordString) extends AnyVal {
    @inline def ++(other: String): CordString = Cord(self + other)
  }
}
