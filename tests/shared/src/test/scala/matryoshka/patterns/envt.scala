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

import exp._
import helpers._
import data.cofree._
import scalacheck.cogen._
import scalacheck.arbitrary._

import cats.data._
import cats.implicits._

import cats.laws.discipline._
import cats.kernel.laws.discipline._

import org.specs2.mutable._

class EnvTSpec extends Specification with AlgebraChecks {
  "EnvT" >> {
    checkAll("EnvT.EqLaws", EqTests[EnvT[String, Exp, Int]].eqv)
    checkAll("EnvT.ComonadLaws", ComonadTests[EnvT[String, NonEmptyList, ?]].comonad[Int, Int, Int])

    checkAlgebraIsoLaws("EnvT ⇔ Cofree", EnvT.cofreeIso[Int, Exp])
  }
}
