package typeclasses

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  /*
   * map(fa)(x => x) == fa
   *
   * map(map(fa)(a =>b))(b => c) === map(fa)(a => b => c)
   *
   */


}

object Functor {


  implicit object OptionFunctor extends Functor[Option] {
    override def map[A, B](oa: Option[A])(f: A => B): Option[B] = oa.map(f)
  }

  implicit def writerFunctor[W] = new Functor[({type Z[A] = Writer[A, W]})#Z] {
    override def map[A, B](wa: Writer[A, W])(f: A => B): Writer[B, W] = wa.map(f)
  }

}



