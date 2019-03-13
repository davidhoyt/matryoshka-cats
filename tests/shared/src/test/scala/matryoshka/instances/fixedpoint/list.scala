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
package instances.fixedpoint

import slamdata.Predef._

import kernel._
import compat._
import helpers._
import data._
import data.list._
import implicits._
import patterns._
import scalacheck.cogen._
import scalacheck.arbitrary._

import cats.implicits._

import cats.laws.discipline._
import cats.kernel.laws.discipline._

import org.scalacheck._
import org.specs2.ScalaCheck
import org.specs2.mutable._
import org.typelevel.discipline.specs2.mutable._

class ListSpec extends Specification with ScalaCheck with Discipline with CatsMatchers {
  "List laws" >> {
    checkAll("List.EqLaws", EqTests[List[Int]].eqv)
    // TODO: Cats, FIX THIS!
    //checkAll("List.FoldableLaws", CompatFoldableTests[List].foldable[Int, Int])
  }


  "apply" should {
    "be equivalent to scala.List.apply" in {
      List(1, 2, 3, 4).cata(ListF.listIso.get) must
        equal(scala.List(1, 2, 3, 4))
    }
  }

  "fill" should {
    "be equivalent to scala.List.fill" >> prop { (n: Nat, v: Int) =>
      List.fill[scala.List[Int]](n)(v) must
        equal(scala.List.fill(n.toInt)(v))
    }
  }

  "length" should {
    "count the number of elements" >> prop { (n: Nat, v: Int) =>
      List.fill[List[Int]](n)(v).length must equal(n.toInt)
    }
  }

  "headOption" should {
    "return the first element" in {
      List(1, 2, 3, 4).headOption must beSome(1)
    }

    "if there is one" in {
      List().headOption must beNone
    }
  }

  "tailOption" should {
    "return the remainder of the list" in {
      List(1, 2, 3, 4).tailOption must equal(List(2, 3, 4).some)
      List(1).tailOption must equal(List[Int]().some)
    }

    "if there is one" in {
      List().tailOption must beNone
    }
  }
}
