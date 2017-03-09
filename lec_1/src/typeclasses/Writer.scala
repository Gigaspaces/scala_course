package typeclasses

import Monoid._

case class Writer[A, W](result: A, count: W) {
  def map[B](f: A => B): Writer[B, W] = Writer(f(result), count)

  def flatMap[B](f: A => Writer[B, W])(implicit mw: Monoid[W]): Writer[B, W] = {
    val Writer(b, newCount) = f(result)
    Writer(b, count |+| newCount)
  }
}

object Writer {
  def tell[W](w: W): Writer[Unit, W] = Writer((), w)
}