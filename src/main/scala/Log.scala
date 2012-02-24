package com.github.voqo.train

object Log {
  import System.err.{println => printerr}

  def error(s: String) { printerr("error: %s" format s) }
  def error(s: String, e: Exception) { error(s); printerr(e.getMessage) }
}
