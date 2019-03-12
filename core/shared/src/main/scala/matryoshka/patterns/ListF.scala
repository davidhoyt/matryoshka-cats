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

import Recursive.ops._

import kernel._
import compat._

import cats._
import cats.syntax.all._
import cats.instances.all._

sealed abstract class ListF[A, B] {
  def headOption: Option[A] = this match {
    case ConsF(h, _) => h.some
    case NilF()      => None
  }

  def tailOption: Option[B] = this match {
    case ConsF(_, t) => t.some
    case NilF()      => None
  }
}
final case class ConsF[A, B](car: A, cdr: B) extends ListF[A, B]
final case class NilF[A, B]() extends ListF[A, B]

object ListF {
  def listIso[A]: AlgebraIso[ListF[A, ?], List[A]] =
    AlgebraIso[ListF[A, ?], List[A]] {
      case ConsF(h, t) => h :: t
      case NilF()      => Nil
    } {
      case h :: t => ConsF(h, t)
      case Nil    => NilF()
    }

  def takeUpTo[N, T, A](implicit N: Recursive.Aux[N, Option], T: Recursive.Aux[T, ListF[A, ?]]): Coalgebra[ListF[A, ?], (N, T)] =
    pair => pair._1.project.fold[ListF[A, (N, T)]](NilF())(p => pair._2.project.map((p, _)))

  def find[A](cond: A => Boolean): Algebra[ListF[A, ?], Option[A]] = {
    case ConsF(h, t) => if (cond(h)) h.some else t
    case NilF()      => None
  }

  implicit def equal[A: Equal]: Delay[Equal, ListF[A, ?]] =
    new Delay[Equal, ListF[A, ?]] {
      def apply[β](eq: Equal[β]) = Equal.equal((a, b) => (a, b) match {
        case (ConsF(h1, t1), ConsF(h2, t2)) => h1 ≟ h2 && eq.equal(t1, t2)
        case (NilF(),        NilF())        => true
        case (_,             _)             => false
      })
    }

  implicit def show[A: Show]: Delay[Show, ListF[A, ?]] =
    new Delay[Show, ListF[A, ?]] {
      def apply[β](show: Show[β]) = Show.show {
        case ConsF(h, t) => h.show ++ Cord("::") ++ show.show(t)
        case NilF()      => Cord("nil")
      }
    }

  @Deviation("Defines bifoldRight and renames bitraverseImpl.")
  implicit def bitraverse: Bitraverse[ListF] = new Bitraverse[ListF] {
    override def bifoldLeft[A, B, C](fab: ListF[A, B], c: C)(f: (C, A) => C, g: (C, B) => C): C =
      fab match {
        case NilF() => c
        case ConsF(a, _) => f(c, a)
      }

    override def bifoldRight[A, B, C](fab: ListF[A, B], c: Eval[C])(f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]): Eval[C] =
      fab match {
        case NilF() => c
        case ConsF(_, b) => g(b, c)
      }

    override def bitraverse[G[_], A, B, C, D](
        fab: ListF[A, B])(
        f: A ⇒ G[C], g: B ⇒ G[D])(
        implicit G: Applicative[G]): G[ListF[C, D]] =
        fab match {
          case NilF()        => G.point(NilF())
          case ConsF(a, b)   => (f(a), g(b)).mapN(ConsF(_, _))
        }
    }

  implicit def traverse[A]: Traverse[ListF[A, ?]] = bitraverse.rightTraverse[A]
}
