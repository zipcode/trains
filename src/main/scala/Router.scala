package com.github.voqo.train

object Router {
  def main(args: Array[String]) {
    if (args.size != 1) {
      Log.error("Wrong number of arguments. Please supply the source directory.")
      System.exit(1)
    }

    val routeing_points = RouteingPoints.fromDirectory(args(0))
    val routeing_point_identifier = RouteingPointIdentifier.fromDirectory(args(0))
    val points = routeing_point_identifier.map(_._2).flatten.toSet
    val permitted_routes = PermittedRouteIdentifier.fromDirectory(args(0), points)

    routeing_points foreach { case (p, m) => println("Map => %s: %s".format(p, m.mkString(", "))) }
    routeing_point_identifier foreach { case (p, m) => println("Point => %s: %s".format(p, m.mkString(", "))) }
    permitted_routes foreach { case (p1, p2, ms) => println("Permitted route => %s - %s: %s".format(p1, p2, ms.mkString(" "))) }
  }
}
