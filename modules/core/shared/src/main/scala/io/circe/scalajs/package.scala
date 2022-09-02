package io.circe

import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.util.control.NonFatal

package object scalajs {

  /**
   * Convert [[scala.scalajs.js.Any]] to [[Json]].
   */
  final def convertJsToJson(input: js.Any): Json =
    input.asInstanceOf[Json]

  /**
   * Decode [[scala.scalajs.js.Any]].
   */
  final def decodeJs[A](input: js.Any)(implicit d: Decoder[A]): Decoder.Result[A] =
    d.decodeJson(convertJsToJson(input))

  /**
   * Convert [[Json]] to [[scala.scalajs.js.Any]].
   */
  final def convertJsonToJs(input: Json): js.Any = input.asInstanceOf[js.Any]

  implicit final class EncoderJsOps[A](private val value: A) extends AnyVal {
    def asJsAny(implicit encoder: Encoder[A]): js.Any = convertJsonToJs(encoder(value))
  }

  implicit final def decodeJsUndefOr[A](implicit d: Decoder[A]): Decoder[js.UndefOr[A]] =
    Decoder[Option[A]].map(_.fold[js.UndefOr[A]](js.undefined)(a => a))

  implicit final def encodeJsUndefOr[A](implicit e: Encoder[A]): Encoder[js.UndefOr[A]] =
    Encoder.instance(_.fold(Json.Null)(e(_)))
}
