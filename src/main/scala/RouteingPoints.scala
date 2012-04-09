package com.github.voqo.train

import java.io.File

object RouteingPoints {
  val targetfile = "routeing_points.pdf"

  def fromDirectory(s: String) = fromPDF(new File(s, targetfile))

  def fromPDF(f: File) = {
    parse(PDFReader.getPages(f) flatMap { _.split("\n").drop(1) })
  }

  /**
   * Extract Routeing Point to Map mappings
   * This relies upon each Map name being two characters, and the map
   * names being listed in alphabetical order.
   * Also: JN is never a token. Don't chomp it.
   */
  private def parse(lines: Seq[String]) = {
    val entries = for (line <- lines) yield {
      val tokens = line split " "
      val ms: List[String] = tokens.foldRight(List[String]()) { (token, acc) =>
        if (token.size == 2 && (acc.isEmpty || token < acc.head) && token != "JN") {
          token :: acc
        } else acc
      }
      val s = tokens take (tokens.size - ms.size) mkString " "
      (s, ms)
    }

    /* One or two entries roll onto the next line.  Fix it up.*/
    entries.foldRight(List[(String, Seq[String])]()) { (item, acc) =>
      if (acc.size > 0 && acc.head._1 == "") {
        (item._1 -> (item._2 ++ acc.head._2)) :: acc.tail
      } else {
        item :: acc
      }
    }
  }
}
