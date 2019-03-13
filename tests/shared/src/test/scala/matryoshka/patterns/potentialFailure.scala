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

import kernel._
import helpers._
import data.Fix

import exp._
import scalacheck.cogen._
import scalacheck.arbitrary._

import cats.implicits._

import cats.laws.discipline._
import cats.kernel.laws.discipline._

import org.specs2.ScalaCheck
import org.specs2.mutable._

import org.scalacheck._
import org.typelevel.discipline.specs2.mutable._

class PotentialFailureSpec extends Specification with ScalaCheck with Discipline {
  implicit def potentialFailureArbitrary[T[_[_]], F[_], E: Arbitrary](
    implicit T: Arbitrary[T[F]], F: Delay[Arbitrary, F]):
      Delay[Arbitrary, PotentialFailure[T, F, E, ?]] =
    new Delay[Arbitrary, PotentialFailure[T, F, E, ?]] {
      def apply[α](arb: Arbitrary[α]) =
        Arbitrary(Gen.oneOf(
          T.arbitrary.map(Success[T, F, E, α](_)),
          Arbitrary.arbitrary[E].map(Failure[T, F, E, α](_)),
          F(arb).arbitrary.map(PartialFailure[T, F, E, α](_))))
    }

  "PotentialFailure" >> {
    checkAll("PotentialFailure.EqLaws", EqTests[PotentialFailure[Fix, Exp, String, Int]].eqv)
    checkAll("PotentialFailure.BitraverseLaws", BitraverseTests[PotentialFailure[Fix, Exp, ?, ?]].bitraverse[Option, Int, Int, Int, String, String, String])
    checkAll("PotentialFailure.TraverseLaws", CompatTraverseTests[PotentialFailure[Fix, Exp, String, ?]].traverse[Int, Int, Int, Int, Option, Option])
  }
}
