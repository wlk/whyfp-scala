object chapter3 {
  sealed abstract class List[+T] {
    def string: String // Required to prevent default `toString` evaluation in Scala Worksheets
  }

  case object Nil extends List[Nothing] {
    override def toString = "Nil"
    override def string: String = toString
  }

  class Cons[T](h: => T, t: => List[T]) extends List[T] {
    val head: T = h
    def tail: List[T] = t
    override def toString = s"Cons($head, ???)"
    override def string = s"Cons($head, ${tail.string})"
  }

  object Cons {
    def apply[T](h: T, tail: => List[T]): Cons[T] = {
      new Cons(h, tail)
    }

    def unapply[T](arg: Cons[T]): Option[(T, List[T])] = {
      Some(arg.head, arg.tail)
    }
  }

  val emptyList = Nil
  val singleElem = Cons(1, Nil)
  val twoElem = Cons(1, Cons(2, Nil))
  val threeElem = Cons(1, Cons(2, Cons(3, Nil)))

  def f(i: Int): List[Int] = {
    if (i == 0) Nil
    else Cons(i, f(i - 1))
  }

  def sum(list: List[Int]): Int = list match {
    case Nil => 0
    case Cons(h, tail) => h + sum(tail)
  }

  def reduce[T](f: ((T, T) => T), z: T, list: List[T]): T = list match {
    case Nil => z
    case Cons(h, t) => f(h, reduce(f, z, t))
  }

  def foldr[T, Y](f: ((T, Y) => Y), z: Y, list: List[T]): Y = list match {
    case Nil => z
    case Cons(h, t) => f(h, foldr(f, z, t))
  }

  def sum2(list: List[Int]) =
    foldr[Int, Int](
      (x: Int, y: Int) => x + y,
      0,
      list
    )

  def product(l: List[Int]) = foldr[Int, Int](_ * _, 1, l)

  def anytrue(l: List[Boolean]) = foldr[Boolean, Boolean](_ || _, false, l)

  def alltrue(l: List[Boolean]) = foldr[Boolean, Boolean](_ && _, true, l)

  def append[T](a: List[T], b: List[T]): List[T] = foldr[T, List[T]](Cons(_, _), b, a)

  def count(a: Any, n: Int) = n + 1

  def length(list: List[Any]) = foldr[Any, Int](count, 0, list)

  def double(n: Int) = n * 2

  def fandcons[T](f: T => T, el: T, list: List[T]) = Cons(f(el), list)

  def doubleall(list: List[Int]) = foldr((x: Int, y: List[Int]) => Cons(double(x), y), Nil, list)

  def map[A, B](list: List[A])(f: A => B): List[B] = foldr((x: A, y: List[B]) => Cons(f(x), y), Nil, list)

  def summatrix(list: List[List[Int]]): Int = sum2(map(list)(sum2))

  def main(args: Array[String]): Unit = {
    f(10).string

    sum(emptyList)
    sum(singleElem)
    sum(twoElem)
    sum(threeElem)

    sum2(emptyList)
    sum2(singleElem)
    sum2(twoElem)
    sum2(threeElem)

    anytrue(Cons(true, Cons(false, Nil)))
    anytrue(Cons(true, Cons(true, Nil)))
    anytrue(Cons(false, Cons(false, Nil)))

    alltrue(Cons(true, Cons(false, Nil)))
    alltrue(Cons(true, Cons(true, Nil)))
    alltrue(Cons(false, Cons(false, Nil)))

    append(threeElem, twoElem).string

    length(threeElem)

    doubleall(threeElem).string

    map[Int, Int](threeElem)(_ + 1337).string

  }

}
