package com.github.voqo.train

import java.io.File
import java.io.OutputStreamWriter
import java.io.ByteArrayOutputStream

import org.apache.pdfbox.pdmodel.PDDocument
import org.apache.pdfbox.pdmodel.PDPage
import org.apache.pdfbox.util.PDFTextStripper

object RouteingPoints {
  def main(args: Array[String]) {
    if (args.size != 1) {
      Log.error("Wrong number of arguments")
      System.exit(1)
    }
    extract(args(0))
  }

  /**
   * Takes the path of routeing_point_identifier.pdf and returns
   * each routeing line within that document
   */
  def extract(s: String): Seq[String] = {
    val sep = "---- PAGE ----\n"
    val doc = PDDocument.load(s)
    val stream = new ByteArrayOutputStream
    val output = new OutputStreamWriter(stream)
    val stripper = new PDFTextStripper("UTF-8") {
      override def startPage(page: PDPage) {
        output.write(sep)
      }
    }
    stripper.writeText(doc, output)
    output.flush
    stream.toString("UTF-8").split(sep).map { _.split("\n").drop(4) }.flatten.toList
  }
}

class RouteingPoints
