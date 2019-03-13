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
package helpers

import scala.collection.Seq
import scala.Predef.ArrowAssoc
import slamdata.Predef.{Option, Some, Nil, Boolean, String}

import cats._
import cats.implicits._
import cats.kernel.CommutativeMonoid

import cats.laws._
import cats.laws.discipline._
import arbitrary.catsLawsArbitraryForPartialFunction

import org.scalacheck.{Arbitrary, Cogen}
import org.scalacheck.Prop._

trait CompatFoldableTests[F[_]] extends UnorderedFoldableTests[F] {
  def laws: FoldableLaws[F]

  def foldable[A: Arbitrary, B: Arbitrary](implicit
                                           ArbFA: Arbitrary[F[A]],
                                           A: CommutativeMonoid[A],
                                           B: CommutativeMonoid[B],
                                           CogenA: Cogen[A],
                                           CogenB: Cogen[B],
                                           EqA: Eq[A],
                                           EqFA: Eq[F[A]],
                                           EqB: Eq[B],
                                           EqOptionB: Eq[Option[B]],
                                           EqOptionA: Eq[Option[A]]): RuleSet =
    new DefaultRuleSet(
      name = "foldable",
      parent = Some(unorderedFoldable[A, B]),
      "foldLeft consistent with foldMap" -> forAll(laws.leftFoldConsistentWithFoldMap[A, B] _),
      "foldRight consistent with foldMap" -> forAll(laws.rightFoldConsistentWithFoldMap[A, B] _),
      "ordered constistency" -> forAll(laws.orderedConsistency[A] _),
      "exists consistent with find" -> forAll(laws.existsConsistentWithFind[A] _),
      //"exists is lazy" -> forAll(laws.existsLazy[A] _),
      //"forall is lazy" -> forAll(laws.forallLazy[A] _),
      "foldM identity" -> forAll(laws.foldMIdentity[A, B] _),
      "reduceLeftOption consistent with reduceLeftToOption" ->
        forAll(laws.reduceLeftOptionConsistentWithReduceLeftToOption[A] _),
      "reduceRightOption consistent with reduceRightToOption" ->
        forAll(laws.reduceRightOptionConsistentWithReduceRightToOption[A] _),
      "get reference" -> forAll(laws.getRef[A] _),
      "fold reference" -> forAll(laws.foldRef[A] _),
      "toList reference" -> forAll(laws.toListRef[A] _),
      "filter_ reference" -> forAll(laws.filter_Ref[A] _),
      "takeWhile_ reference" -> forAll(laws.takeWhile_Ref[A] _),
      "dropWhile_ reference" -> forAll(laws.dropWhile_Ref[A] _),
      "collectFirstSome reference" -> forAll(laws.collectFirstSome_Ref[A, B] _)
      //"collectFirst reference" -> forAll(laws.collectFirst_Ref[A, B] _)
    )
}

object CompatFoldableTests {
  def apply[F[_]: Foldable]: CompatFoldableTests[F] =
    new CompatFoldableTests[F] { def laws: FoldableLaws[F] = FoldableLaws[F] }
}
