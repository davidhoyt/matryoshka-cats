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
package example

import kernel._
import compat._

import slamdata.Predef._

import cats._
import cats.kernel.Eq
import cats.implicits._

sealed abstract class Example[A]
final case class Empty[A]()                                   extends Example[A]
final case class NonRec[A](a: String, b: Int)                 extends Example[A]
final case class SemiRec[A](a: Int, b: A)                     extends Example[A]
final case class MultiRec[A](a: A, b: A)                      extends Example[A]
final case class OneList[A](l: List[A])                       extends Example[A]
final case class TwoLists[A](first: List[A], second: List[A]) extends Example[A]

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
object Example {
  implicit val traverse: Traverse[Example] =
    new Traverse[Example] {
      override def foldLeft[A, B](fa: Example[A], b: B)(f: (B, A) => B): B = ???

      override def foldRight[A, B](fa: Example[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = ???

      override def traverse[G[_], A, B](fa: Example[A])(f: A => G[B])(implicit G: Applicative[G]): G[Example[B]] =
        fa match {
          case Empty()        => G.point(Empty())
          case NonRec(a, b)   => G.point(NonRec(a, b))
          case SemiRec(a, b)  => f(b).map(SemiRec(a, _))
          case MultiRec(a, b) => (f(a), f(b)).mapN(MultiRec(_, _))
          case OneList(a)     => a.traverse(f).map(OneList(_))
          case TwoLists(a, b) => (a.traverse(f), b.traverse(f)).mapN(TwoLists(_, _))
        }
    }

  implicit val equal: Delay[Eq, Example] =
    new Delay[Eq, Example] {
      override def apply[A](eq: Eq[A]): Eq[Example[A]] = {
        implicit val ieq = eq
        Eq.instance {
          case (Empty(), Empty())                   => true
          case (NonRec(s1, i1), NonRec(s2, i2))     => (s1 ≟ s2) && (i1 ≟ i2)
          case (SemiRec(i1, a1),  SemiRec(i2, a2))  => (i1 ≟ i2) && eq.eqv(a1, a2)
          case (MultiRec(a1, b1), MultiRec(a2, b2)) => eq.eqv(a1, a2) && eq.eqv(b1, b2)
          case (OneList(l),       OneList(r))       => l ≟ r
          case (TwoLists(l1, l2), TwoLists(r1, r2)) => (l1 ≟ r1) && (l2 ≟ r2)
          case (_,                _)                => false
        }
      }
    }

  implicit val show: Delay[Show, Example] =
    new Delay[Show, Example] {
      override def apply[A](s: Show[A]): Show[Example[A]] = {
        implicit val is = s
        Show.show {
          case Empty()          => show"Empty()"
          case NonRec(s2, i2)   => show"NonRec($s2, $i2)"
          case SemiRec(i2, a2)  => show"SemiRec($i2, $a2)"
          case MultiRec(a2, b2) => show"MultiRec($a2, $b2)"
          case OneList(r)       => show"OneList($r)"
          case TwoLists(r1, r2) => show"TwoLists($r1, $r2)"
        }
      }
    }

}
