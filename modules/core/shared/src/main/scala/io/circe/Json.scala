package io.circe

import cats.{ Eq, Show }
import io.circe.numbers.BiggerDecimal
import java.io.Serializable
import scala.collection.mutable.ListBuffer

import scala.scalajs.js
import scala.scalajs.js.JSConverters._

/**
 * A data type representing possible JSON values.
 *
 * @author Travis Brown
 * @author Tony Morris
 * @author Dylan Just
 * @author Mark Hibberd
 */
opaque type Json <: Matchable = js.Any

object Json {

  extension (json: Json)

    /**
     * Reduce this JSON value with the given [[Json.Folder]].
     */
    def foldWith[X](folder: Json.Folder[X]): X =
      json match {
        case JNull       => folder.onNull
        case JBoolean(b) => folder.onBoolean(b)
        case JNumber(n)  => folder.onNumber(n)
        case JString(s)  => folder.onString(s)
        case JArray(a)   => folder.onArray(a)
        case JObject(o)  => folder.onObject(o)
      }

    /**
     * The catamorphism for the JSON value data type.
     */
    final def fold[X](
      jsonNull: => X,
      jsonBoolean: Boolean => X,
      jsonNumber: JsonNumber => X,
      jsonString: String => X,
      jsonArray: Vector[Json] => X,
      jsonObject: JsonObject => X
    ): X = json match {
      case JNull       => jsonNull
      case JBoolean(b) => jsonBoolean(b)
      case JNumber(n)  => jsonNumber(n)
      case JString(s)  => jsonString(s)
      case JArray(a)   => jsonArray(a)
      case JObject(o)  => jsonObject(o)
    }

    /**
     * Run on an array or object or return the given default.
     */
    final def arrayOrObject[X](
      or: => X,
      jsonArray: Vector[Json] => X,
      jsonObject: JsonObject => X
    ): X = json match {
      case JNull       => or
      case JBoolean(_) => or
      case JNumber(_)  => or
      case JString(_)  => or
      case JArray(a)   => jsonArray(a)
      case JObject(o)  => jsonObject(o)
    }

    /**
     * Construct a successful cursor from this JSON value.
     */
    final def hcursor: HCursor = HCursor.fromJson(json)

    def isNull: Boolean = json == null
    def isBoolean: Boolean = JBoolean.unapply(json).isDefined
    def isNumber: Boolean = JNumber.unapply(json).isDefined
    def isString: Boolean = JString.unapply(json).isDefined
    def isArray: Boolean = JArray.unapply(json).isDefined
    def isObject: Boolean = JObject.unapply(json).isDefined

    def asNull: Option[Unit] = Some(json).collect { case JNull => () }
    def asBoolean: Option[Boolean] = Some(json).collect { case JBoolean(b) => b }
    def asNumber: Option[JsonNumber] = Some(json).collect { case JNumber(n) => n }
    def asString: Option[String] = Some(json).collect { case JString(s) => s }
    def asArray: Option[Vector[Json]] = Some(json).collect { case JArray(a) => a }
    def asObject: Option[JsonObject] = Some(json).collect { case JObject(o) => o }

    def withNull(f: => Json): Json = asNull.fold(json)(_ => f)
    def withBoolean(f: Boolean => Json): Json = asBoolean.fold(json)(f)
    def withNumber(f: JsonNumber => Json): Json = asNumber.fold(json)(f)
    def withString(f: String => Json): Json = asString.fold(json)(f)
    def withArray(f: Vector[Json] => Json): Json = asArray.fold(json)(f)
    def withObject(f: JsonObject => Json): Json = asObject.fold(json)(f)

    def mapBoolean(f: Boolean => Boolean): Json = asNull.fold(json)(_ => f)
    def mapNumber(f: JsonNumber => JsonNumber): Json = asNumber.fold(json)(f(_))
    def mapString(f: String => String): Json = asString.fold(json)(f(_))
    def mapArray(f: Vector[Json] => Vector[Json]): Json = asArray.fold(json)(f(_))
    def mapObject(f: JsonObject => JsonObject): Json = asObject.fold(json)(o => JObject(f(o)))

    /**
     * The name of the type of the JSON value.
     */
    final def name: String =
      json match {
        case JNull       => "Null"
        case JBoolean(_) => "Boolean"
        case JNumber(_)  => "Number"
        case JString(_)  => "String"
        case JArray(_)   => "Array"
        case JObject(_)  => "Object"
      }

    /**
     * Attempts to decode this JSON value to another data type.
     */
    final def as[A](implicit d: Decoder[A]): Decoder.Result[A] = d(hcursor)

    /**
     * Pretty-print this JSON value to a string using the given pretty-printer.
     */
    final def printWith(p: Printer): String = p.print(json)

    /**
     * Pretty-print this JSON value to a string with no spaces.
     */
    final def noSpaces: String = Printer.noSpaces.print(json)

    /**
     * Pretty-print this JSON value to a string indentation of two spaces.
     */
    final def spaces2: String = Printer.spaces2.print(json)

    /**
     * Pretty-print this JSON value to a string indentation of four spaces.
     */
    final def spaces4: String = Printer.spaces4.print(json)

    /**
     * Pretty-print this JSON value to a string with no spaces, with object keys
     * sorted alphabetically.
     */
    final def noSpacesSortKeys: String = Printer.noSpacesSortKeys.print(json)

    /**
     * Pretty-print this JSON value to a string indentation of two spaces, with
     * object keys sorted alphabetically.
     */
    final def spaces2SortKeys: String = Printer.spaces2SortKeys.print(json)

    /**
     * Pretty-print this JSON value to a string indentation of four spaces, with
     * object keys sorted alphabetically.
     */
    final def spaces4SortKeys: String = Printer.spaces4SortKeys.print(json)

    /**
     * Perform a deep merge of this JSON value with another JSON value.
     *
     * Objects are merged by key, values from the argument JSON take
     * precedence over values from this JSON. Nested objects are
     * recursed.
     *
     * Null, Array, Boolean, String and Number are treated as values,
     * and values from the argument JSON completely replace values
     * from this JSON.
     */
    def deepMerge(that: Json): Json =
      (asObject, that.asObject) match {
        case (Some(lhs), Some(rhs)) =>
          fromJsonObject(
            lhs.toIterable.foldLeft(rhs) {
              case (acc, (key, value)) =>
                rhs(key).fold(acc.add(key, value)) { r =>
                  acc.add(key, value.deepMerge(r))
                }
            }
          )
        case _ => that
      }

    /**
     * Drop the entries with a null value if this is an object.
     *
     * Note that this does not apply recursively.
     */
    def dropNullValues: Json = mapObject(_.filter { case (_, v) => !v.isNull })

    /**
     * Drop the entries with a null value if this is an object or array.
     */
    def deepDropNullValues: Json = {
      val folder = new Json.Folder[Json] {
        def onNull: Json = Json.Null
        def onBoolean(value: Boolean): Json = Json.fromBoolean(value)
        def onNumber(value: JsonNumber): Json = Json.fromJsonNumber(value)
        def onString(value: String): Json = Json.fromString(value)
        def onArray(value: Vector[Json]): Json =
          Json.fromValues(value.collect {
            case v if !v.isNull => v.foldWith(this)
          })
        def onObject(value: JsonObject): Json =
          Json.fromJsonObject(
            value.filter { case (_, v) => !v.isNull }.mapValues(_.foldWith(this))
          )
      }

      foldWith(folder)
    }

    /**
     * Drop the entries with an empty value if this is an array or object.
     *
     * Note that this does not apply recursively.
     */
    def dropEmptyValues: Json = mapObject(_.filter {
      case (_, JArray(vec))  => vec.nonEmpty
      case (_, JObject(obj)) => obj.nonEmpty
      case _                 => true
    })

    /**
     * Compute a `String` representation for this JSON value.
     */
    final def toString: String = spaces2

    /**
     * Universal equality derived from our type-safe equality.
     */
    final def equals(that: Any): Boolean =
      Json.eqJson.eqv(json, that.asInstanceOf[js.Any])

    /**
     * Use implementations provided by case classes.
     */
    def hashCode(): Int = json.hashCode()

    // Alias for `findAllByKey`.
    final def \\(key: String): List[Json] = findAllByKey(key)

    /**
     * Recursively return all values matching the specified `key`.
     *
     * The Play docs, from which this method was inspired, reads:
     *   "Lookup for fieldName in the current object and all descendants."
     */
    final def findAllByKey(key: String): List[Json] = {
      val hh: ListBuffer[Json] = ListBuffer.empty[Json]
      def loop(json: Json): Unit = json match {
        case JObject(obj) =>
          obj.toIterable.foreach {
            case (k, v) =>
              if (k == key) hh += v
              loop(v)
          }
        case JArray(elems) => elems.foreach(loop)
        case _             => // do nothing
      }
      loop(json)
      hh.toList
    }

  /**
   * Represents a set of operations for reducing a [[Json]] instance to a value.
   */
  trait Folder[X] extends Serializable {
    def onNull: X
    def onBoolean(value: Boolean): X
    def onNumber(value: JsonNumber): X
    def onString(value: String): X
    def onArray(value: Vector[Json]): X
    def onObject(value: JsonObject): X
  }

  final val JNull: Json = null

  object JBoolean {
    def unapply(value: Json): Option[Boolean] =
      value.asInstanceOf[Any] match {
        case b: Boolean => Some(b)
        case _          => None
      }
  }

  object JNumber {
    def unapply(value: Json): Option[Double] =
      value.asInstanceOf[Any] match {
        case b: Double => Some(b)
        case _         => None
      }
  }

  object JString {
    def unapply(value: Json): Option[String] =
      value.asInstanceOf[Any] match {
        case s: String => Some(s)
        case _         => None
      }
  }

  object JArray {
    def apply(values: Iterable[Json]): Json =
      values.toJSArray

    def unapply(value: Json): Option[Vector[Json]] =
      value match {
        case a: js.Array[Json] @unchecked => Some(a.toVector)
        case _                            => None
      }
  }

  object JObject {
    def apply(o: JsonObject): Json = o.asInstanceOf[Json]

    def unapply(value: Json): Option[JsonObject] =
      value match {
        case o: js.Object => Some(o.asInstanceOf[JsonObject])
        case _            => None
      }
  }

  final val Null: Json = JNull
  final val True: Json = true
  final val False: Json = false

  /**
   * Create a `Json` value representing a JSON object from key-value pairs.
   */
  final def obj(fields: (String, Json)*): Json = fromFields(fields)

  /**
   * Create a `Json` value representing a JSON array from values.
   */
  final def arr(values: Json*): Json = fromValues(values)

  /**
   * Create a `Json` value representing a JSON object from a collection of key-value pairs.
   */
  final def fromFields(fields: Iterable[(String, Json)]): Json =
    JsonObject.fromIterable(fields).asInstanceOf[Json]

  /**
   * Create a `Json` value representing a JSON array from a collection of values.
   */
  final def fromValues(values: Iterable[Json]): Json = JArray(values.toVector)

  /**
   * Create a `Json` value representing a JSON object from a [[JsonObject]].
   */
  final def fromJsonObject(value: JsonObject): Json = JObject(value)

  /**
   * Create a `Json` value representing a JSON number from a [[JsonNumber]].
   */
  final def fromJsonNumber(value: JsonNumber): Json = value

  /**
   * Create a `Json` value representing a JSON string.
   *
   * Note that this does not parse the argument.
   */
  final def fromString(value: String): Json = value

  /**
   * Create a `Json` value representing a JSON boolean.
   */
  final def fromBoolean(value: Boolean): Json = value

  /**
   * Create a `Json` value representing a JSON number from an `Int`.
   */
  final def fromInt(value: Int): Json = value.toDouble

  /**
   * Create a `Json` value representing a JSON number from a `Long`.
   */
  final def fromLong(value: Long): Json = value.toDouble

  /**
   * Try to create a `Json` value representing a JSON number from a `Double`.
   *
   * The result is empty if the argument cannot be represented as a JSON number.
   */
  final def fromDouble(value: Double): Option[Json] = if (isReal(value)) Some(value) else None

  /**
   * Try to create a `Json` value representing a JSON number from a `Float`.
   *
   * The result is empty if the argument cannot be represented as a JSON number.
   */
  final def fromFloat(value: Float): Option[Json] = if (isReal(value)) Some(value.toDouble) else None

  /**
   * Create a `Json` value representing a JSON number or null from a `Double`.
   *
   * The result is a JSON null if the argument cannot be represented as a JSON
   * number.
   */
  final def fromDoubleOrNull(value: Double): Json = if (isReal(value)) value else null

  /**
   * Create a `Json` value representing a JSON number or null from a `Float`.
   *
   * The result is a JSON null if the argument cannot be represented as a JSON
   * number.
   */
  final def fromFloatOrNull(value: Float): Json = if (isReal(value)) value.toDouble else null

  /**
   * Create a `Json` value representing a JSON number or string from a `Double`.
   *
   * The result is a JSON string if the argument cannot be represented as a JSON
   * number.
   */
  final def fromDoubleOrString(value: Double): Json =
    if (isReal(value)) value else fromString(java.lang.Double.toString(value))

  /**
   * Create a `Json` value representing a JSON number or string from a `Float`.
   *
   * The result is a JSON string if the argument cannot be represented as a JSON
   * number.
   */
  final def fromFloatOrString(value: Float): Json =
    if (isReal(value)) value.toDouble else fromString(java.lang.Float.toString(value))

  /**
   * Create a `Json` value representing a JSON number from a `BigInt`.
   */
  // final def fromBigInt(value: BigInt): Json = JNumber(
  //   JsonBiggerDecimal(BiggerDecimal.fromBigInteger(value.underlying), value.toString)
  // )

  /**
   * Create a `Json` value representing a JSON number from a `BigDecimal`.
   */
  // final def fromBigDecimal(value: BigDecimal): Json = JNumber(JsonBigDecimal(value.underlying))

  /**
   * Calling `.isFinite` directly on the value boxes; we explicitly avoid that here.
   */
  private[this] def isReal(value: Double): Boolean = java.lang.Double.isFinite(value)

  /**
   * Calling `.isFinite` directly on the value boxes; we explicitly avoid that here.
   */
  private[this] def isReal(value: Float): Boolean = java.lang.Float.isFinite(value)

  private[this] final def arrayEq(x: Seq[Json], y: Seq[Json]): Boolean = {
    val it0 = x.iterator
    val it1 = y.iterator
    while (it0.hasNext && it1.hasNext) {
      if (Json.eqJson.neqv(it0.next(), it1.next())) return false
    }
    it0.hasNext == it1.hasNext
  }

  implicit final val eqJson: Eq[Json] = Eq.instance {
    case (JObject(a), JObject(b))   => JsonObject.eqJsonObject.eqv(a, b)
    case (JString(a), JString(b))   => a == b
    case (JNumber(a), JNumber(b))   => a == b
    case (JBoolean(a), JBoolean(b)) => a == b
    case (JArray(a), JArray(b))     => arrayEq(a.toSeq, b.toSeq)
    case (x, y)                     => x.isNull && y.isNull
  }

  implicit final val showJson: Show[Json] = Show.fromToString[Json]
}
