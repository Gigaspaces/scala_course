
object showmons extends App {

  import typeclasses.Monoid._


  val json: Map[String, List[Int]] = Map("a" -> List(10), "b" -> List(20, 5), "c" -> List(15, 7))
  //> json  : Map[String,List[Int]] = Map(a -> List(10), b -> List(20, 5), c -> Li
  //| st(15, 7))

  json ++ json //> res0: scala.collection.immutable.Map[String,List[Int]] = Map(a -> List(10),
  //| b -> List(20, 5), c -> List(15, 7))

  println(json ++ json)
  println(json |+| json)
  // same as println(json.|+|(json))

  val strs = List("a", "b", "c") //> strs  : List[String] = List(a, b, c)


  strs.foldMap(_ => json) //> res1: Map[String,List[Int]] = Map(a -> List(10, 10, 10), b -> List(20, 5, 20
  //| , 5, 20, 5), c -> List(15, 7, 15, 7, 15, 7))

  val ints = List(1, 2, 3)

  def identity[A](a: A): A = a

  println(ints.foldMap(identity))
}
