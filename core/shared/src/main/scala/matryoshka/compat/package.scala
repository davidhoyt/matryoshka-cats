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

import cats._
import cats.syntax.all._

package object compat
  extends IdSyntax
    with DisjunctionSyntax
    with SemigroupSyntax
    with MonoidSyntax
    with ApplicativeSyntax
    with FunctorSyntax
    with FlatMapSyntax
    with CoflatMapSyntax
    with ComonadSyntax
    with TraverseSyntax
    with BitraverseSyntax
    with BifoldableSyntax
    with ComposeSyntax
    with CofreeSyntax
    with NaturalTransformationSyntax
    with EqualSyntax
    with EitherTSyntax
    with ShowSyntax
    with OrderSyntax
    with OptionSyntax
    with OptionTSyntax
    with UnzipSyntax
    with TagSyntax
    with BooleanSyntax
    with Tuple2Syntax
    with Tuple3Syntax
    with FreeSyntax {

    //  def unfold[F[_], A](a: A)(f: A => F[A])(implicit F: Functor[F]): Cofree[F, A] =
    //    ana(a)(f, x => x)
    //
    //  def ana[F[_], A, B](a: A)(coalg: A => F[A], f: A => B)(implicit F: Functor[F]): Cofree[F, B] =
    //    anaEval(a)(a => Eval.later(coalg(a)), f)
    //
    //  def anaEval[F[_], A, B](a: A)(coalg: A => Eval[F[A]], f: A => B)(implicit F: Functor[F]): Cofree[F, B] =
    //    Cofree[F, B](f(a), mapSemilazy(coalg(a))(fa => F.map(fa)(anaEval(_)(coalg, f))))
    //
    //  private def mapSemilazy[A, B](fa: Eval[A])(f: A => B): Eval[B] = fa match {
    //    case Now(a) => Now(f(a))
    //    case other  => other.map(f)
    //  }
    //
    //  def myUnfold[F[_], A, B](b: B)(f: B => (A, F[B]))(implicit F: Functor[F]): Cofree[F, A] = {
    //    //ana[F, B, (A, F[B])]
    //    val (a: A, fb: F[B]) = f(b)
    //    val lifted: F[B] => F[Cofree[F, A]] = F.lift[B, Cofree[F, A]](b2 => myUnfold[F, A, B](b2)(f))
    //    Cofree[F, A](a, Eval.now[F[B]](fb).map[F[Cofree[F, A]]](lifted))
    //  }
    //
    //  def unfold[F[_], A, B](b: B)(f: B => (A, F[B]))(implicit F: Functor[F], T: Functor[λ[a => Free[Function0, a]]]): Cofree[F, A] = {
    //    val (a, fb) = f(b)
    //    val zzz = F.lift(unfold(_)(f))
    //    val nt = T.map(Trampoline.done(fb))(z => F.lift(unfold(_)(f)))
    //    Cofree.applyT(a, nt)
    //  }
    //
    //  //creates an instance of Cofree that trampolines all of the calls to the tail so we get stack safety
    //  def applyT[S[_],A](a: A, tf: Free[Function0,S[Cofree[S,A]]])(implicit T: Functor[λ[a => Free[Function0, a]]]): Cofree[S, A] =
    //    new Cofree[S,A] {
    //
    //      def head = a
    //
    //      def t = tf
    //
    //      def applyCofree[B](f: A => B, g: Cofree[S, A] => Cofree[S, B])(implicit S: Functor[S]): Cofree[S,B] =
    //        applyT(f(head), T.map(t)(S.lift(g)))
    //    }

    // F[Eval[A]] ~> Eval[F[A]]
    def distEval[F[_]: Functor]: DistributiveLaw[F, Eval] =
      new DistributiveLaw[F, Eval] {
        override def apply[A](fa: F[Eval[A]]): Eval[F[A]] =
          Eval.later(fa map (_.value))
      }

    // Eval[F[A]] ~> F[Eval[F[A]]
    def distEvalI[F[_]: Functor]: DistributiveLaw[Eval, F] =
      new DistributiveLaw[Eval, F] {
        override def apply[A](evalA: Eval[F[A]]): F[Eval[A]] =
          evalA.value.map(Eval.now)
      }

    type FEval[F[_], H[_], Z] = F[Eval[H[Z]]]
    type EvalK[F[_], H[_], Z] = Eval[F[H[Z]]]

    def safeDistEvalN[F[_], H[_], A](given: DistributiveLaw[F, H])(fEvalH: FEval[F, H, A])(implicit F: Functor[F]): EvalK[H, F, A] = {
      val step1: Eval[H[F[A]]] = Eval.later(given(F.map(fEvalH)(_.value)))
      step1
    }

    def safeDistEvalK[F[_], H[_], A](given: DistributiveLaw[F, H])(fEvalH: FEval[F, H, A])(implicit F: Functor[F], H: Functor[H]): Eval[FEval[H, F, A]] = {
      val step1: Eval[H[F[A]]] = safeDistEvalN(given)(fEvalH)
      val step2: Eval[H[Eval[F[A]]]] = step1.map(hfa => H.map(hfa)(fa => Eval.now(fa)))
      step2
    }

    def unsafeDistEvalK[F[_], H[_], A](given: DistributiveLaw[F, H])(fEvalH: FEval[F, H, A])(implicit F: Functor[F], H: Functor[H]): FEval[H, F, A] = {
      val step1: F[H[A]] = F.map(fEvalH)(evalHA => evalHA.value)
      val step2: H[F[A]] = given(step1)
      val step3: H[Eval[F[A]]] = H.map(step2)(fa => Eval.now(fa))
      step3
    }
}
