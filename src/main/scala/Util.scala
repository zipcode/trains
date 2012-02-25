package com.github.voqo.train

object Util {
  /** Standard unfold */
  def unfold[S,T](init: T)(f: T => Option[(S, T)]): List[S] = f(init) match {
    case Some((item, remain)) => item :: unfold(remain)(f)
    case None => Nil
  }
}
