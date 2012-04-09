package com.github.voqo.train

import java.io.File
import java.io.ByteArrayOutputStream
import java.io.OutputStreamWriter

import org.apache.pdfbox.pdmodel.PDDocument
import org.apache.pdfbox.pdmodel.PDPage
import org.apache.pdfbox.util.PDFTextStripper

object PDFReader {
  def getPages(f: File): Seq[String] = {
    val sep = "---- PAGE ----\n"
    val doc = PDDocument.load(f)
    val stream = new ByteArrayOutputStream
    val output = new OutputStreamWriter(stream)
    val stripper = new PDFTextStripper("UTF-8") {
      override def startPage(page: PDPage) {
        output.write(sep)
      }
    }
    stripper.writeText(doc, output)
    output.flush
    doc.close
    stream.toString("UTF-8").split(sep).drop(1)
  }
}
