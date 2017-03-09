
object showcounters extends App {

  import typeclasses.Writer

  def foo(s: String): Writer[String, Int] =
    Writer(s.toUpperCase + s, 1)

  def bar(s: String): Writer[String, Int] =
    Writer(s + s.toUpperCase, 1)

  def goo(s: String): String = s

  for {
    r1 <- foo("hello")
    r2 <- foo("world")
    r8 = goo("sdfsf")
    r3 <- bar(r1)
    r4 <- bar(r3 + r2)
  } yield r4

  def gcd(a: Int, b: Int): Writer[Int, List[(Int, Int)]] = {
    if (b == 0) Writer(a, List((a, b))) else
      for {
        _ <- Writer.tell(List((a, b)))
        r <- gcd(b, a % b)
      } yield r
    //same as: Writer.tell(List((a, b))).flatMap(_ => gcd(b, a % b).map(r => r)) // Writer(4,List((16,12), (12,4), (4,0)))
  }

  println(gcd(16, 12))


  def fact(a: Int): Writer[Int, List[Int]] = {
    if (a == 0) Writer(1, Nil) else for {
      _ <-Writer.tell(List(a))
      r <-  fact(a - 1)
    } yield a * r
  }

  println(fact(3))
}




