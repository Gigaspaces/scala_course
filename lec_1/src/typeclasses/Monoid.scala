package typeclasses
import Equal._
trait Monoid[A] {

  def append(a1: A, a2: A): A
  def zero: A

  //def multiply(count: Int, a: A) : A = List.fill(count)(a).
  
  
  object Laws {
    def leftZero(a: A)(implicit ea: Equal[A]): Boolean = append(zero, a) === a

    def rightZero(a: A)(implicit ea: Equal[A]): Boolean = append(a, zero) === a

    def assoc(a1: A, a2: A, a3: A)(implicit ea: Equal[A]): Boolean = append(append(a1, a2), a3) === append(a1, append(a2, a3))
  }
}

object Monoid {
  implicit class MonoidOps[A](val a1: A) extends AnyVal {
    def |+|(a2: A)(implicit ma: Monoid[A]): A = ma.append(a1, a2)
  }
  
  implicit class RichTraversable[A](val traversable: Traversable[A]) extends AnyVal {
    def foldMap[B : Monoid](f: A => B): B = traversable.foldLeft(implicitly[Monoid[B]].zero){
      (acc, e) => acc |+| f(e)
    }
  }
  

  implicit object IntAdditionMon extends Monoid[Int] {
    override def append(i1: Int, i2: Int): Int = i1 + i2
    override def zero = 0
  }
  implicit object BooleanAdditionMon extends Monoid[Boolean] {
    override def append(i1: Boolean, i2: Boolean): Boolean = i1 || i2
    override def zero = false
  }
  implicit object StringMon extends Monoid[String] {
    override def append(i1: String, i2: String): String = i1 + i2
    override def zero = ""
  }

  implicit def listMon[A] = new Monoid[List[A]] {
    override def append(i1: List[A], i2: List[A]): List[A] = i1 ++ i2
    override def zero = Nil
  }

  implicit def optionMon[A : Monoid] = new Monoid[Option[A]] {
    override def append(oa1: Option[A], oa2: Option[A]): Option[A] = for {
      a1 <- oa1
      a2 <- oa2
    } yield a1 |+| a2
    override def zero = Some(implicitly[Monoid[A]].zero)
  }
  
  implicit def unaryOpMon[A] = new Monoid[A => A] {
    override def zero: (A) => A = identity
    override def append(f1: A => A, f2: A => A): (A) => A = f1 andThen f2
  }
  
  
  implicit def mapMon[K,V : Monoid] = new Monoid[Map[K,V]] {
    override def zero: Map[K, Nothing] = Map.empty
    override def append(bm1: Map[K,V], sm2: Map[K,V]) : Map[K,V] = {
      /// check lengths and choose the big/small
      sm2.foldLeft(bm1){
        (acc, e) => {
            val (k1,v1) = e
            acc + (k1 -> ( acc.getOrElse(k1, implicitly[Monoid[V]].zero)  |+| v1))
        }
      }
    }
  }

}