package io.circe

import cats.kernel.instances.string._
import cats.syntax.eq._
import io.circe.tests.PrinterSuite

class Spaces4PrinterSuite extends PrinterSuite(Printer.spaces4, parser.`package`)
class UnicodeEscapePrinterSuite extends PrinterSuite(Printer.noSpaces.copy(escapeNonAscii = true), parser.`package`) {
  import io.circe.syntax._
  test("Printing object should unicode-escape all non-ASCII chars") {
    val actual = Json.obj("0 ℃" := "32 ℉").printWith(printer)
    val expected = "{\"0 \\u2103\":\"32 \\u2109\"}"
    assert(actual === expected)
  }
}

class Spaces2PrinterWithWriterReuseSuite
    extends PrinterSuite(
      Printer.spaces2.copy(reuseWriters = true),
      parser.`package`
    )

class Spaces4PrinterWithWriterReuseSuite
    extends PrinterSuite(
      Printer.spaces4.copy(reuseWriters = true),
      parser.`package`
    )

class NoSpacesPrinterWithWriterReuseSuite
    extends PrinterSuite(
      Printer.noSpaces.copy(reuseWriters = true),
      parser.`package`
    )

class UnicodeEscapePrinterWithWriterReuseSuite
    extends PrinterSuite(
      Printer.noSpaces.copy(reuseWriters = true, escapeNonAscii = true),
      parser.`package`
    )
