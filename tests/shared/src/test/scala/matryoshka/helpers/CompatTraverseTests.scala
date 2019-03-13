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
import slamdata.Predef.{Option, Nil, Boolean, String}

import cats._
import cats.implicits._
import cats.kernel.CommutativeMonoid

import cats.laws._
import cats.laws.discipline._

import org.scalacheck.{Arbitrary, Cogen, Prop}
import Prop._

trait CompatTraverseTests[F[_]] extends FunctorTests[F] with CompatFoldableTests[F] with UnorderedTraverseTests[F] {
  def laws: TraverseLaws[F]

  def traverse[A: Arbitrary, B: Arbitrary, C: Arbitrary, M: Arbitrary, X[_]: CommutativeApplicative, Y[_]: CommutativeApplicative](
    implicit
    ArbFA: Arbitrary[F[A]],
    ArbFB: Arbitrary[F[B]],
    ArbXB: Arbitrary[X[B]],
    ArbXM: Arbitrary[X[M]],
    ArbYB: Arbitrary[Y[B]],
    ArbYC: Arbitrary[Y[C]],
    ArbYM: Arbitrary[Y[M]],
    ArbFXM: Arbitrary[F[X[M]]],
    CogenA: Cogen[A],
    CogenB: Cogen[B],
    CogenC: Cogen[C],
    CogenM: Cogen[M],
    M: CommutativeMonoid[M],
    MA: CommutativeMonoid[A],
    EqFA: Eq[F[A]],
    EqFC: Eq[F[C]],
    EqM: Eq[M],
    EqA: Eq[A],
    EqXYFC: Eq[X[Y[F[C]]]],
    EqXFB: Eq[X[F[B]]],
    EqYFB: Eq[Y[F[B]]],
    EqXFM: Eq[X[F[M]]],
    EqYFM: Eq[Y[F[M]]],
    EqOptionA: Eq[Option[A]]
  ): RuleSet = {
    implicit def EqXFBYFB: Eq[(X[F[B]], Y[F[B]])] = new Eq[(X[F[B]], Y[F[B]])] {
      override def eqv(x: (X[F[B]], Y[F[B]]), y: (X[F[B]], Y[F[B]])): Boolean =
        EqXFB.eqv(x._1, y._1) && EqYFB.eqv(x._2, y._2)
    }
    new RuleSet {
      def name: String = "traverse"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(functor[A, B, C], foldable[A, M], unorderedTraverse[A, M, C, X, Y])
      def props: Seq[(String, Prop)] = Seq(
        "traverse identity" -> forAll(laws.traverseIdentity[A, C] _),
        "traverse sequential composition" -> forAll(laws.traverseSequentialComposition[A, B, C, X, Y] _),
        "traverse parallel composition" -> forAll(laws.traverseParallelComposition[A, B, X, Y] _),
        "traverse derive foldMap" -> forAll(laws.foldMapDerived[A, M] _),
        "traverse order consistency" -> forAll(laws.traverseOrderConsistent[A] _),
        "traverse ref mapWithIndex" -> forAll(laws.mapWithIndexRef[A, C] _),
        "traverse ref traverseWithIndexM" -> forAll(laws.traverseWithIndexMRef[Option, A, C] _),
        "traverse ref zipWithIndex" -> forAll(laws.zipWithIndexRef[A, C] _)
      )
    }
  }
}

object CompatTraverseTests {
  def apply[F[_]: Traverse]: CompatTraverseTests[F] =
    new CompatTraverseTests[F] { def laws: TraverseLaws[F] = TraverseLaws[F] }
}
