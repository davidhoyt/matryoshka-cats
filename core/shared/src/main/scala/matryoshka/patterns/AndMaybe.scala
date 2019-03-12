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
import compat._

import cats._
import cats.implicits._

import monocle.Iso

sealed abstract class AndMaybe[A, B] {
  def head: A = this match {
    case Indeed(a, _) => a
    case Only(a)      => a
  }

  def tailOption: Option[B] = this match {
    case Indeed(_, b) => some(b)
    case Only(_)      => none
  }
}
final case class Indeed[A, B](h: A, t: B) extends AndMaybe[A, B]
final case class Only[A, B](a: A)         extends AndMaybe[A, B]

object AndMaybe extends AndMaybeInstances {
  def envTIso[A, B]: Iso[AndMaybe[A, B], EnvT[A, Option, B]] =
    Iso[AndMaybe[A, B], EnvT[A, Option, B]] {
      case Indeed(h, t) => EnvT((h, t.some))
      case Only(h)      => EnvT((h, none))
    } (envt => envt.lower.fold[AndMaybe[A, B]](Only(envt.ask))(Indeed(envt.ask, _)))

  def find[A](p: A => Boolean): Algebra[AndMaybe[A, ?], Option[A]] =
    l => if (p(l.head)) some(l.head) else l.tailOption.join
}

sealed abstract class AndMaybeInstances {
  implicit def traverse[A]: Traverse[AndMaybe[A, ?]] =
    bitraverse.rightTraverse[A]

  @Deviation("Defines bifoldRight and renames bitraverseImpl.")
  implicit val bitraverse: Bitraverse[AndMaybe] =
    new Bitraverse[AndMaybe] {

      override def bifoldLeft[A, B, C](fab: AndMaybe[A, B], c: C)(f: (C, A) => C, g: (C, B) => C): C =
        fab match {
          case Only(a) => f(c, a)
          case Indeed(a, b) => g(f(c, a), b)
        }

      override def bifoldRight[A, B, C](fab: AndMaybe[A, B], c: Eval[C])(f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]): Eval[C] =
        fab match {
          case Only(a) => f(a, c)
          case Indeed(a, b) => g(b, f(a, c))
        }

      override def bitraverse[G[_]: Applicative, A, B, C, D](fab: AndMaybe[A, B])(f: A => G[C], g: B => G[D]): G[AndMaybe[C, D]] =
        fab match {
          case Indeed(a, b) => (f(a), g(b)).mapN(Indeed(_, _))
          case Only(a)    => f(a) map (Only(_))
        }
    }

  implicit def equal[A: Equal]: Delay[Equal, AndMaybe[A, ?]] =
    new Delay[Equal, AndMaybe[A, ?]] {
      def apply[B](eql: Equal[B]): Equal[AndMaybe[A, B]] = {
        implicit val eqlB: Equal[B] = eql
        Equal.equalBy {
          case Indeed(a, b) => (a, b).right[A]
          case Only(a)    => a.left[(A, B)]
        }
      }
    }

  implicit def show[A: Show]: Delay[Show, AndMaybe[A, ?]] =
    new Delay[Show, AndMaybe[A, ?]] {
      def apply[B](show: Show[B]) =
        Show.show {
          case Indeed(a, b) => a.show ++ Cord("::") ++ show.show(b)
          case Only(a)    => a.show
        }
    }
}
