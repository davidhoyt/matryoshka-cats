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
package patterns

import slamdata.Predef._

import helpers._
import scalacheck.cogen._
import scalacheck.arbitrary._

import cats.implicits._
import cats.laws.discipline.BitraverseTests
import cats.kernel.laws.discipline.EqTests

import org.specs2.ScalaCheck
import org.specs2.mutable._

class AndMaybeSpec extends Specification with ScalaCheck with AlgebraChecks {
  //import cats.kernel.instances.all._
  "AndMaybe" >> {
    checkAll("AndMaybe.EqLaws", EqTests[AndMaybe[String, Int]].eqv)
    checkAll("AndMaybe.BitraverseLaws", BitraverseTests[AndMaybe].bitraverse[Option, Int, Int, Int, String, String, String])
  }
}
