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
package instances

import slamdata.Predef._

import compat._

import data._
import implicits._
import patterns._

import monocle.Prism

import cats._
import cats.implicits._

/** This package provides instances of various common data structures
  * implemented explicitly as fixed-points.
  */
package object fixedpoint {
  /** Natural numbers represented as the least fixed-point of [[scala.Option]].
    */
  type Nat = Mu[Option]

  object Nat {
    def zero[N](implicit N: Corecursive.Aux[N, Option]): N = none[N].embed

    def succ[N](prev: N)(implicit N: Corecursive.Aux[N, Option]): N =
      some(prev).embed

    def one[N](implicit N: Corecursive.Aux[N, Option]): N = succ(zero)

    val fromInt: CoalgebraM[Option, Option, Int] =
      x => if (x < 0) None else Some(if (x > 0) (x - 1).some else None)

    // NB: This isn’t defined via `AlgebraPrism` because it only holds across a
    //     recursive structure.
    def intPrism[T](implicit T: Birecursive.Aux[T, Option]): Prism[Int, T] =
      Prism[Int, T](_.anaM[T](fromInt))(_.cata(height))
  }

  implicit class RecursiveOptionOps[T]
    (self: T)
    (implicit T: Recursive.Aux[T, Option]) {
    def toInt: Int = self.cata(height)
  }

  implicit class CorecursiveOptionOps[T]
    (self: T)
    (implicit T: Corecursive.Aux[T, Option]) {
    def succ: T = Nat.succ(self)
  }

  implicit class BirecursiveOptionOps[T]
    (self: T)
    (implicit T: Birecursive.Aux[T, Option]) {
    def +(other: T): T = other.cata[T] {
      case None => self
      case o    => o.embed
    }
  }

  /** The dual of [[Nat]], a potentially-infinite number. */
  type Conat = Nu[Option]
  object Conat {
    /** A representation of infinity, as a non-terminating corecursive process */
    def inf[N](implicit N: Corecursive.Aux[N, Option]): N = ().ana[N](_.some)
  }

  type Free[F[_], A]   = Mu[CoEnv[A, F, ?]]
  type Cofree[F[_], A] = Mu[EnvT[A, F, ?]]
  type List[A]         = Mu[ListF[A, ?]]

  object List {
    def apply[A](elems: A*): List[A] =
      elems.toList.ana[Mu[ListF[A, ?]]](ListF.listIso[A].reverseGet)

    def tuple[A](elem: => A): Option ~> ListF[A, ?] =
      λ[Option ~> ListF[A, ?]] {
        case None    => NilF()
        case Some(b) => ConsF(elem, b)
      }

    def forget[A]: ListF[A, ?] ~> Option =
      λ[ListF[A, ?] ~> Option] {
        case NilF()      => None
        case ConsF(_, t) => t.some
      }

//    object fill {
//      def apply[L]: PartiallyApplied[L] = new PartiallyApplied[L]
//
//      class PartiallyApplied[L] {
//        def apply[N, A]
//          (n: N)
//          (elem: => A)
//          (implicit N: Recursive.Aux[N, Option], L: Corecursive.Aux[L, ListF[A, ?]])
//            : L =
//          n.transAna[L](tuple(elem))
//      }
//    }
  }
}
