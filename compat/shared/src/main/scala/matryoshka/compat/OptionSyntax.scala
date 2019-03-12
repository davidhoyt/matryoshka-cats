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

trait OptionSyntax {
  import OptionSyntax._

  /** [[scala.Some.apply]] with a sometimes more convenient type. */
  final def some[A](a: A): Option[A] = Some(a)

  @inline implicit final def toCompatOptionOps[A](maybe: Option[A]): OptionOps[A] = new OptionOps[A](maybe)
}

object OptionSyntax {
  final class OptionOps[A](private val self: Option[A]) extends AnyVal {
    @inline def \/>[E](e: => E): E \/ A = self.fold[E \/ A](Left(e))(Right(_))
  }
}
