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

import Tag.{Conjunction, Disjunction}

trait BooleanSyntax {
  import BooleanSyntax._

  implicit val disjunctionMonoid: Monoid[WithDisjunction] = BooleanSyntax.disjunctionMonoid

  implicit val conjunctionMonoid: Monoid[WithConjunction] = BooleanSyntax.conjunctionMonoid

  @inline implicit final def toCompatBooleanOps(self: Boolean): BooleanOps = new BooleanOps(self)
}

@SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
object BooleanSyntax {
  type WithDisjunction = Boolean with Disjunction
  type WithConjunction = Boolean with Conjunction

  val disjunctionMonoid: Monoid[WithDisjunction] =
    new Monoid[WithDisjunction] {
      override val empty: WithDisjunction = false.asInstanceOf[WithDisjunction]
      override def combine(f1: WithDisjunction, f2: WithDisjunction): WithDisjunction = (f1 || f2).asInstanceOf[WithDisjunction]
    }

  val conjunctionMonoid: Monoid[WithConjunction] =
    new Monoid[WithConjunction] {
      override val empty: WithConjunction = true.asInstanceOf[WithConjunction]
      override def combine(f1: WithConjunction, f2: WithConjunction): WithConjunction = (f1 && f2).asInstanceOf[WithConjunction]
    }

  final class BooleanOps(private val self: Boolean) extends AnyVal {
    @inline def conjunction: WithConjunction = self.asInstanceOf[WithConjunction]
    @inline def disjunction: WithDisjunction = self.asInstanceOf[WithDisjunction]

    /**
      * @return `t` if true, `f` otherwise
      */
    @inline def fold[A](t: => A, f: => A): A = if (self) t else f
  }
}
