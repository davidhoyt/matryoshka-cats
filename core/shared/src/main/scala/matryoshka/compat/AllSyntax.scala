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

trait AllSyntax
  extends IdSyntax
    with DisjunctionSyntax
    with TheseSyntax
    with NonEmptyListSyntax
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
    with FreeSyntax
    with FoldableSyntax
