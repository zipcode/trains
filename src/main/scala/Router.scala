package com.github.voqo.train

object Router {
  def main(args: Array[String]) {
    if (args.size != 1) {
      Log.error("Wrong number of arguments. Please supply the source directory.")
      System.exit(1)
    }

    val routeing_points = RouteingPoints.fromDirectory(args(0))
    val routeing_point_identifier = RouteingPointIdentifier.fromDirectory(args(0))

    routeing_points foreach { case (p, m) => println("Map => %s: %s".format(p, m.mkString(", "))) }
    routeing_point_identifier foreach { case (p, m) => println("Point => %s: %s".format(p, m.mkString(", "))) }
  }
}
