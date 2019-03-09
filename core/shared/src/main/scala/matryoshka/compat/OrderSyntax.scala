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

import cats.kernel._

trait OrderSyntax {
  import OrderSyntax._

  type Ordering = OrderSyntax.Ordering

  val Ordering: OrderSyntax.Ordering.type = OrderSyntax.Ordering

  @inline implicit final def toCompatOrderOps[F](given: F): OrderOps[F] = new OrderOps[F](given)
  @inline implicit final def toCompatOrderObjectOps(given: Order.type): OrderObjectOps.type = OrderObjectOps
}

object OrderSyntax {
  type Ordering = Comparison

  object Ordering {
    val EQ: Comparison.EqualTo.type = Comparison.EqualTo
  }

  object OrderObjectOps {
    def order[A](f: (A, A) => Ordering): Order[A] =
      (a1: A, a2: A) => f(a1, a2).toInt
  }

  implicit class OrderOps[F](private val self: F) extends AnyVal {
    def ?|?(other: F)(implicit F: Order[F]): Ordering = F.comparison(self, other)
  }
}

