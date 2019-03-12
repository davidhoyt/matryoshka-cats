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

trait TheseSyntax {
  type \&/[+A, +B] = TheseSyntax.\&/[A, B]
}

object TheseSyntax {
  type \&/[+A, +B] = Ior[A, B]

  object Both {
    @inline def apply[A, B](a: A, b: B): A \&/ B = Ior.Both(a, b)
  }

  object This {
    @inline def apply[A](a: A): A \&/ Nothing = Ior.Left(a)
  }

  object That {
    @inline def apply[B](b: B): Nothing \&/ B = Ior.Right(b)
  }
}
