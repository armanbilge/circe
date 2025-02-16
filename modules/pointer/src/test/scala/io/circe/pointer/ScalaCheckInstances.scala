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

package io.circe.pointer

import org.scalacheck._

private[pointer] trait ScalaCheckInstances {

  /**
   * Generates a `String` which is a valid Object or Array reference according
   * to RFC 6901 for JSON pointers.
   *
   * The primary purpose of this generator is to handle escapes correctly.
   */
  final val genPointerReferenceString: Gen[String] =
    Arbitrary.arbitrary[String].map(_.replaceAll("~", "~0").replaceAll("/", "~1"))
}

object ScalaCheckInstances extends ScalaCheckInstances
