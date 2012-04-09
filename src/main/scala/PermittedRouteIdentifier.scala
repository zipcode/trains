package com.github.voqo.train

import java.io.File

object PermittedRouteIdentifier {
  val targetfile = "permitted_route_identifier.pdf"

  def fromDirectory(s: String, p: Set[String]) = fromPDF(new File(s, targetfile), p)

  def fromPDF(f: File, p: Set[String]) = {
    parse(PDFReader.getPages(f) flatMap { _.split("\n").drop(1) }, p)
  }

  private def parse(lines: Seq[String], points: Set[String]): Seq[(String, String, Seq[String])] = {
    val splitlines = for (line <- lines) yield {
      /* Chomp known routeing points off the start. */
      val tokens = Util.unfold(line) { ss =>
        points find {p: String => ss.toLowerCase startsWith (p.toLowerCase + " ")} match {
          case Some(s) => Some(s, ss drop (s.size + 1))
          case None => None
        }
      }
      /* What remains will be maps */
      if (tokens.size == 0) {
        (tokens, line)
      } else {
        (tokens, line drop (tokens.mkString(" ", "", " ").size))
      }
    }

    /* Some things are split onto new lines. Tack 'em on */
    val entries = splitlines.foldRight(List[(Seq[String], String)]()) { (item, acc) =>
      if (acc.size > 0 && acc.head._1 == Nil) {
        (item._1 -> (item._2 ++ " " ++ acc.head._2)) :: acc.tail
      } else {
        item :: acc
      }
    }

    /* Fix it up to be the right datatype */
    entries map { case (stations, points) =>
      val List(p1, p2) = stations
      (p1, p2, points.split(" +").toSeq)
    }       
  }
}
