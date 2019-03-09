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

trait Tuple3Syntax {
  import Tuple3Syntax._

  @inline implicit final def toCompatTuple3Instance[A1, A2]: Functor[(A1, A2, ?)] = new Tuple3Functor[A1, A2]
}

object Tuple3Syntax {
  private[compat] class Tuple3Functor[A1, A2] extends Functor[(A1, A2, ?)] {
    override final def map[A, B](fa: (A1, A2, A))(f: A => B): (A1, A2, B) =
      (fa._1, fa._2, f(fa._3))
  }
}
