/*
 * Copyright 2023 circe
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

package io.circe

import io.circe.tests.CirceSuite
import org.scalacheck.{ Arbitrary, Gen }
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{ Millis, Span }
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class MemoizedPiecesSuite extends CirceSuite with ScalaFutures {
  implicit val defaultPatience: PatienceConfig = PatienceConfig(timeout = Span(1000, Millis))

  case class Depths(depths: Seq[Int])

  object Depths {
    implicit val arbitraryDepths: Arbitrary[Depths] = Arbitrary(
      Gen.containerOfN[Seq, Int](512, Gen.choose(0, 256)).map(Depths(_))
    )
  }

  def makePieces: Printer.MemoizedPieces = new Printer.MemoizedPieces(" ") {
    def compute(i: Int): Printer.Pieces = new Printer.Pieces(
      "%s%s%s".format(" " * i, "a", " " * (i + 1)),
      "%s%s%s".format(" " * i, "b", " " * (i + 1)),
      "%s%s%s".format(" " * i, "c", " " * (i + 1)),
      "%s%s%s".format(" " * i, "d", " " * (i + 1)),
      "%s%s%s".format(" " * i, "e", " " * (i + 1)),
      "%s%s%s".format(" " * i, "f", " " * (i + 1)),
      "%s%s%s".format(" " * i, "g", " " * (i + 1)),
      "%s%s%s".format(" " * i, "h", " " * (i + 1))
    )
  }

  val pieces: Printer.MemoizedPieces = {
    val tmp = makePieces
    makePieces(256)
    tmp
  }

  "Printer.MemoizedPieces" should "should be correct for arbitrarily ordered depths under concurrent usage" in {
    forAll { (depths: Depths) =>
      val newPieces = makePieces

      whenReady(Future.traverse(depths.depths)(depth => Future(newPieces(depth)))) { result =>
        assert(result.sameElements(depths.depths.map(pieces(_))))
      }
    }
  }
}
