package io.circe

import cats.{ Applicative, Eq, Foldable, Show, Traverse }
import cats.data.Kleisli
import cats.syntax.all._
import java.io.Serializable
import java.util.LinkedHashMap
import scala.collection.immutable.Map

import scala.scalajs.js
import scala.scalajs.js.JSConverters._

/**
 * A mapping from keys to JSON values that maintains insertion order.
 *
 * @author Travis Brown
 *
 * @groupname Contents Operations for accessing contents
 * @groupprio Contents 0
 *
 * @groupname Conversions Conversions to other collection types
 * @groupprio Conversions 1
 *
 * @groupname Modification Operations that transform the JSON object
 * @groupprio Modification 2
 *
 * @groupname Other Equality and other operations
 * @groupprio Other 3
 */
opaque type JsonObject <: Matchable = js.Object

/**
 * Constructors, type class instances, and other utilities for [[JsonObject]].
 */
object JsonObject {

  extension (obj: JsonObject)
    private def dict: js.Dictionary[Json] = obj.asInstanceOf[js.Dictionary[Json]]

    /**
     * Return the JSON value associated with the given key, with undefined behavior if there is none.
     *
     * @group Contents
     */
    private[circe] def applyUnsafe(k: String): Json =
      dict(k)

    /**
     * Return the JSON value associated with the given key.
     *
     * @group Contents
     */
    def apply(key: String): Option[Json] =
      dict.get(key)

    /**
     * Return `true` if there is an association with the given key.
     *
     * @group Contents
     */
    def contains(key: String): Boolean =
      dict.contains(key)

    /**
     * Return the number of associations.
     *
     * @group Contents
     */
    def size: Int = dict.size

    /**
     * Return `true` if there are no associations.
     *
     * @group Contents
     */
    def isEmpty: Boolean = dict.isEmpty

    /**
     * Return `true` if there is at least one association.
     *
     * @group Contents
     */
    final def nonEmpty: Boolean = !isEmpty

    /**
     * Return a Kleisli arrow that gets the JSON value associated with the given field.
     *
     * @group Contents
     */
    final def kleisli: Kleisli[Option, String, Json] = Kleisli(apply(_))

    /**
     * Return all keys in insertion order.
     *
     * @group Contents
     */
    def keys: Iterable[String] = dict.keys

    /**
     * Return all associated values in insertion order.
     *
     * @group Contents
     */
    def values: Iterable[Json] = dict.values

    /**
     * Convert to a map.
     *
     * @note This conversion does not maintain insertion order.
     * @group Conversions
     */
    def toMap: Map[String, Json] = dict.toMap

    /**
     * Return all key-value pairs in insertion order.
     *
     * @group Conversions
     */
    def toIterable: Iterable[(String, Json)] = dict.toList

    /**
     * Return all key-value pairs in insertion order as a list.
     *
     * @group Conversions
     */
    final def toList: List[(String, Json)] = toIterable.toList

    /**
     * Return all key-value pairs in insertion order as a vector.
     *
     * @group Conversions
     */
    final def toVector: Vector[(String, Json)] = toIterable.toVector

    private def modifyClone(f: js.Dictionary[Json] => Unit): JsonObject = {
      val o = js.Object.create(obj)
      f(o.asInstanceOf[js.Dictionary[Json]])
      o
    }

    /**
     * Insert the given key and value.
     *
     * @group Modification
     */
    def add(key: String, value: Json): JsonObject =
      modifyClone(_(key) = value)

    /**
     * Prepend the given key-value pair.
     *
     * @group Modification
     */
    def +:(field: (String, Json)): JsonObject = add(field._1, field._2)

    /**
     * Remove the field with the given key (if it exists).
     *
     * @group Modification
     */
    def remove(key: String): JsonObject =
      modifyClone(_.remove(key))

    /**
     * Traverse [[Json]] values.
     *
     * @group Modification
     */
    def traverse[F[_]](f: Json => F[Json])(implicit F: Applicative[F]): F[JsonObject] =
      Traverse[List]
        .traverse(js.Object.entries(obj).toList) { kv =>
          f(kv._2.asInstanceOf[Json]).map(js.Tuple2(kv._1, _))
        }
        .map(e => js.Object.fromEntries(e.toJSArray).asInstanceOf[js.Object])

    /**
     * Transform all associated JSON values.
     *
     * @group Modification
     */
    def mapValues(f: Json => Json): JsonObject =
      js.Object.fromEntries {
        js.Object.entries(obj).map {
          case js.Tuple2(k, v) =>
            js.Tuple2(k, f(v.asInstanceOf[Json]))
          case _ => throw new AssertionError
        }
      }.asInstanceOf[js.Object]

    /**
     * Filter by keys and values.
     *
     * @group Modification
     */
    final def filter(pred: ((String, Json)) => Boolean): JsonObject = JsonObject.fromIterable(toIterable.filter(pred))

    /**
     * Filter by keys.
     *
     * @group Modification
     */
    final def filterKeys(pred: String => Boolean): JsonObject = filter(field => pred(field._1))

    /**
     * Perform a deep merge of this JSON object with another JSON object.
     *
     * Objects are merged by key, values from the argument JSON take
     * precedence over values from this JSON. Nested objects are
     * recursed.
     *
     * See [[Json.deepMerge]] for behavior of merging values that are not objects.
     */
    def deepMerge(that: JsonObject): JsonObject =
      toIterable.foldLeft(that) {
        case (acc, (key, value)) =>
          that(key).fold(acc.add(key, value)) { r =>
            acc.add(key, value.deepMerge(r))
          }
      }

    private[circe] def appendToFolder(folder: Printer.PrintingFolder): Unit =
      folder.writer.append(js.JSON.stringify(obj))

    /**
     * @group Other
     */
    final def toString: String = toIterable.map {
      case (k, v) => s"$k -> ${Json.showJson.show(v)}"
    }.mkString("object[", ",", "]")

    /**
     * @group Other
     */
    final def equals(that: Any): Boolean = that match {
      case that: JsonObject => toMap == that.toMap
      case _                => false
    }

    /**
     * @group Other
     */
    final def hashCode: Int = toMap.hashCode

  /**
   * Construct a [[JsonObject]] from the given key-value pairs.
   */
  final def apply(fields: (String, Json)*): JsonObject = fromIterable(fields)

  /**
   * Construct a [[JsonObject]] from a foldable collection of key-value pairs.
   */
  final def fromFoldable[F[_]](fields: F[(String, Json)])(implicit F: Foldable[F]): JsonObject =
    F.foldLeft(fields, empty) { case (acc, (key, value)) => acc.add(key, value) }

  /**
   * Construct a [[JsonObject]] from an [[scala.collection.Iterable]] (provided for optimization).
   */
  final def fromIterable(fields: Iterable[(String, Json)]): JsonObject =
    js.Object
      .fromEntries(fields.map {
        case (k, v) =>
          js.Tuple2(k, v)
      }.toJSIterable)
      .asInstanceOf[js.Object]

  /**
   * Construct a [[JsonObject]] from a map from keys to [[Json]] values.
   *
   * Note that the order of the fields is arbitrary.
   */
  final def fromMap(map: Map[String, Json]): JsonObject =
    js.Object
      .fromEntries(map.map {
        case (k, v) =>
          js.Tuple2(k, v)
      }.toJSIterable)
      .asInstanceOf[js.Object]

  /**
   * Construct an empty [[JsonObject]].
   */
  final val empty: JsonObject = new js.Object

  /**
   * Construct a [[JsonObject]] with a single field.
   */
  final def singleton(key: String, value: Json): JsonObject =
    js.Dictionary(key -> value).asInstanceOf[js.Object]

  implicit final val showJsonObject: Show[JsonObject] = Show.fromToString
  implicit final val eqJsonObject: Eq[JsonObject] = Eq.fromUniversalEquals

}
