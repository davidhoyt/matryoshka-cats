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
import cats.free._

trait CofreeSyntax {
  import CofreeSyntax._

  @inline implicit final def toCompatCofreeObjectOps(obj: Cofree.type): CofreeObjectOps = cofreeObjectOpsInstance

  @inline implicit final def toCompatCofreeInstanceOps[S[_], A](instance: Cofree[S, A]): CofreeInstanceOps[S, A] = new CofreeInstanceOps[S, A](instance)
}

object CofreeSyntax {
  private[CofreeSyntax] val cofreeObjectOpsInstance = new CofreeObjectOps(Cofree)

  final class CofreeInstanceOps[S[_], A](private val instance: Cofree[S, A]) extends AnyVal {
    def copure: A = instance.head
  }

  final class CofreeObjectOps(private val obj: Cofree.type) extends AnyVal {
    @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
    def compatUnfoldLazy[F[_], A, B](b: B)(f: B => (A, Eval[F[B]]))(implicit F: Functor[F]): Cofree[F, A] = {
      val (a, evalFb) = f(b)
      Cofree[F, A](a, evalFb.map[F[Cofree[F, A]]](fb => F.map(fb)(nextB => compatUnfoldLazy[F, A, B](nextB)(f))))
    }

//    def compatUnfold[F[_], A, B](b: B)(f: B => (A, F[B]))(implicit F: Functor[F]): Cofree[F, A] = {
//      val (a, fb) = f(b)
//      val lifted: F[B] => F[Cofree[F, A]] = F.lift[B, Cofree[F, A]](b2 => compatUnfold[F, A, B](b2)(f))
//      Cofree[F, A](a, Eval.now[F[B]](fb).map[F[Cofree[F, A]]](lifted))
//    }

    @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
    def compatUnfoldF[F[_], A, B](b: B)(f: B => (A, F[Eval[B]]))(implicit F: Functor[F]): Cofree[F, A] = {
      val (a, fEvalB) = f(b)
      val next: Eval[F[Cofree[F, A]]] = Eval.later(F.map(fEvalB)(evalB => compatUnfoldF(evalB.value)(f)))
      Cofree[F, A](a, next)
    }
  }
}
