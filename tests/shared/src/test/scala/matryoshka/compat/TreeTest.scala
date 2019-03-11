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
import org.specs2.mutable._

import cats.implicits._

class TreeTest extends Specification {
  import Tree._

  val E = Equal[Tree[Int]]

  def genTree(size: Int): Tree[Int] =
    (1 to size).foldLeft(Leaf(0))((x, y) => Node(y, Stream(x)))

  val size = 100000

  val deepTree = genTree(size)

  "Tree" >> {
    "deep foldMap should not cause a stack overflow" >> {
      deepTree.foldMap(x => x) must_== (1 to size).sum
    }

    "deep Tree flatten should not cause a stack overflow" >> {
      deepTree.flatten must_== (size to 0 by -1).toStream
    }

    "deep Equal.equal should not cause a stack overflow" >> {
      E.equal(deepTree, deepTree) must_== true
    }

    "deep flatMap should not cause a stack overflow" >> {
      E.equal(deepTree.flatMap(Leaf(_)), deepTree) must_== true
    }
  }
}
