package io.circe.testing

import io.circe.JsonNumber
import scala.scalajs.js.JSON
import scala.util.Try

/**
 * We only want to generate arbitrary [[JsonNumber]] values that Scala.js can
 * parse.
 */
private[testing] trait ArbitraryJsonNumberTransformer {
  def transformJsonNumber(n: JsonNumber): JsonNumber = n
}
