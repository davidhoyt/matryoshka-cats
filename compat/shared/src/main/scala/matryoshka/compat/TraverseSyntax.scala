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
import cats.data._
import cats.kernel.CommutativeMonoid

trait TraverseSyntax {
  import TraverseSyntax._

  type TraverseWithFolds[F[_]] = TraverseSyntax.TraverseWithFolds[F]

  @inline implicit final def toCompatTraverseOps[F[_], A](fa: F[A]): TraverseOps[F, A] = new TraverseOps[F, A](fa)
}

object TraverseSyntax {
  private object internal {
    /** Traverse with `State`. */
    def traverseS[F[_]: Traverse, S, A, B](fa: F[A])(f: A => State[S,B]): State[S,F[B]] =
      traverseSTrampoline[F, S, Id, A, B](fa)(f)

    def runTraverseS[F[_]: Traverse, S, A, B](fa: F[A], s: S)(f: A => State[S, B]): (S, F[B]) =
      traverseS(fa)(f).run(s).value

    /** Traverse `fa` with a `State[S, G[B]]`, internally using a `Trampoline` to avoid stack overflow. */
    def traverseSTrampoline[F[_]: Traverse, S, G[_] : Applicative, A, B](fa: F[A])(f: A => State[S, G[B]]): State[S, G[F[B]]] = {
      import Free._
      import cats.instances.function._
      implicit val A = IndexedStateT.catsDataMonadForIndexedStateT[Trampoline, S].compose(Applicative[G])
      State[S, G[F[B]]](s => {
        val st = Traverse[F].traverse[λ[α => StateT[Trampoline, S, G[α]]], A, B](fa)(a => StateT[Trampoline, S, G[B]](s => Free.defer(Free.pure(f(a).run(s).value))))
        st.run(s).run
      })
    }

    def mapAccumL[F[_]: Traverse, S, A, B](fa: F[A], z: S)(f: (S, A) => (S, B)): (S, F[B]) =
      runTraverseS(fa, z)(a => for {
        s1 <- State.get[S]
        (s2, b) = f(s1, a)
        _ <- State.set(s2)
      } yield b)

    def foldLShape[F[_]: Traverse, A, B](fa: F[A], z: B)(f: (B, A) => B): (B, F[Unit]) =
      runTraverseS(fa, z)(a => State.modify(f(_, a)))

    def foldLeft[F[_]: Traverse, A, B](fa: F[A], z: B)(f: (B, A) => B): B =
      foldLShape(fa, z)(f)._1

    def foldMap[F[_]: Traverse, A, B](fa: F[A])(f: A => B)(implicit F: Monoid[B]): B =
      foldLShape(fa, F.empty)((b, a) => F.combine(b, f(a)))._1

    def foldRight[F[_]: Traverse, A, B](fa: F[A], z: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      foldMap[F, A, Eval[B] => Eval[B]](fa)(a => eb => f(a, eb)) apply z
  }

  final class TraverseOps[F[_], A](private val fa: F[A]) extends AnyVal {
    def zipWith[B, C](fb: F[B])(f: (A, Option[B]) => C)(implicit F: Traverse[F]): (List[B], F[C]) =
      internal.runTraverseS(fa, F.toList(fb))(a => for {
        bs <- State.get
        _ <- State.set(if (bs.isEmpty) bs else bs.drop(1))
      } yield f(a, bs.headOption))

    def zipWithL[B, C](fb: F[B])(f: (A, Option[B]) => C)(implicit F: Traverse[F]): F[C] =
      zipWith(fb)(f)._2

    def zipWithR[B, C](fb: F[B])(f: (Option[A], B) => C)(implicit F: Traverse[F]): F[C] =
      fb.zipWith[A, C](fa)((b, oa) => f(oa, b))._2

    def mapAccumL[S, B](z: S)(f: (S, A) => (S, B))(implicit F: Traverse[F]): (S, F[B]) =
      internal.mapAccumL(fa, z)(f)
  }

  trait TraverseWithFolds[F[_]] extends Traverse[F] { self =>
    override def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B = internal.foldLeft(fa, b)(f)(self)
    override def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = internal.foldRight(fa, lb)(f)(self)
  }
}
