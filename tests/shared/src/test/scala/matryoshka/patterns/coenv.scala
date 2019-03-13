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
import data.free._
import exp.Exp
import exp2.Exp2
import scalacheck.cogen._
import scalacheck.arbitrary._

import cats.implicits._

import cats.laws.discipline._
import cats.kernel.laws.discipline._

import org.specs2.mutable._

class CoEnvSpec extends Specification with AlgebraChecks {
  "CoEnv" >> {
    checkAll("CoEnv.EqLaws", EqTests[CoEnv[String, Exp, Int]].eqv)
    checkAll("CoEnv.BitraverseLaws", BitraverseTests[CoEnv[?, Exp, ?]].bitraverse[Option, Int, Int, Int, String, String, String])
    checkAll("CoEnv.TraverseLaws", CompatTraverseTests[CoEnv[Int, Exp, ?]].traverse[Int, Int, Int, Int, Option, Option])
    // NB: This is to test the low-prio Bi-functor/-foldable instances, so if
    //     Exp2 gets a Traverse instance, this needs to change.
    checkAll("CoEnv.BifunctorLaws", BifunctorTests[CoEnv[?, Exp2, ?]].bifunctor[Int, Int, Int, String, String, String])
    checkAll("CoEnv.FunctorLaws", FunctorTests[CoEnv[Int, Exp2, ?]].functor[Int, String, String])
    checkAll("CoEnv.BifoldableLaws", BifoldableTests[CoEnv[?, Exp2, ?]].bifoldable[Int, Int, String])
    checkAll("CoEnv.FoldableLaws", CompatFoldableTests[CoEnv[Int, Exp2, ?]].foldable[Int, Int])
    // FIXME: These instances don’t fulfill the laws
    // monad.laws[CoEnv[String, Option, ?]].check(Test.Parameters.default)
    // monad.laws[CoEnv[String, NonEmptyList, ?]].check(Test.Parameters.default)

    checkAlgebraIsoLaws("CoEnv ⇔ Free", CoEnv.freeIso[Int, Exp])
  }
}
