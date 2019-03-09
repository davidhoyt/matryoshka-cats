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

import scala.specialized

trait TagSyntax {
  type @@[A, T] = TagSyntax.@@[A, T]

  val Tag: TagSyntax.Tag.type = TagSyntax.Tag
}

object TagSyntax {
  type @@[A, T] = A with T

  /** @see `Tag.of` */
  final class TagOf[T] private[TagSyntax]() {
    /** Like `Tag.apply`, but specify only the `T`. */
    def apply[A](a: A): A @@ T = Tag.apply(a)
  }

  object Tag {
    /** Variants of `apply`, `subst`, and `unsubst` that require
      * specifying the tag type but are more likely to infer the other
      * type parameters.
      */
    def of[T]: TagOf[T] = new TagOf[T]

    /** `subst` specialized to `Id`.
      *
      * @todo According to Miles, @specialized doesn't help here. Maybe manually specialize.
      */
    @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
    @inline def apply[@specialized A, T](a: A): A @@ T = a.asInstanceOf[A @@ T]

    /** `unsubst` specialized to `Id`. */
    @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
    @inline def unwrap[@specialized A, T](a: A @@ T): A = a.asInstanceOf[A]

      /** Type tag to choose a [[cats.Monoid]] instance that performs disjunction (`||`) */
    sealed trait Disjunction

    val Disjunction: TagOf[Disjunction] = Tag.of[Disjunction]

    /** Type tag to choose a [[cats.Monoid]] instance that performs conjunction (`&&`) */
    sealed trait Conjunction

    val Conjunction: TagOf[Conjunction] = Tag.of[Conjunction]
  }
}


