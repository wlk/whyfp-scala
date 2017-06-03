object chapter4_1 {
  // Rewrite of the Cons from chapter 3 to make Cons infinite
  // not really required but makes following functions slightly simpler
  // otherwise I would need to add additional checks ensuring that `X.tail is not Nil`
  class Cons[T](h: => T, t: => Cons[T]) {
    val head: T = h
    def tail: Cons[T] = t
    override def toString = s"Cons($head, ???)"
    def string: String = s"Cons($head, ${tail.string})"
  }

  object Cons {
    def apply[T](h: T, tail: => Cons[T]): Cons[T] = {
      new Cons(h, tail)
    }

    def unapply[T](arg: Cons[T]): Option[(T, Cons[T])] = {
      Some(arg.head, arg.tail)
    }
  }

  def next(n: Double)(x: Double) = (x + n / x) / 2

  def repeat[A](f: A => A, a: A): Cons[A] = Cons(a, repeat(f, f(a)))

  def within(eps: Double, list: Cons[Double]): Double = {
    val a = list.head
    val rest = list.tail
    val b = rest.head
    if (Math.abs(a - b) <= eps) {
      b
    } else {
      within(eps, rest)
    }
  }

  // redefined map for infinite Cons
  def map[A, B](f: A => B)(list: Cons[A]): Cons[B] = list match {
    case c: Cons[A] => Cons(f(c.head), map(f)(c.tail))
  }

  def foldr[T, Y](f: ((T, Y) => Y), list: Cons[T]): Y = list match {
    case Cons(h, t) => f(h, foldr(f, t))
  }

  def sqrt(a0: Double, eps: Double, n: Int): Double = {
    within(eps, repeat(next(n), a0))
  }

  def relative(eps: Double, list: Cons[Double]): Double = {
    val a = list.head
    val rest = list.tail
    val b = rest.head
    if (Math.abs(a / b - 1) <= eps) b else relative(eps, rest)
  }

  def relativesqrt(a0: Double, eps: Double, n: Int): Double = {
    relative(eps, repeat(next(n), a0))
  }

  def main(args: Array[String]): Unit = {
    println(sqrt(1, 0.0001, 2)) // 1.4142135623746899
    println(sqrt(1, 0.0001, 9)) // 3.000000001396984
    println()
    println(relativesqrt(1, 0.0001, 2)) // 1.4142135623746899
    println(relativesqrt(1, 0.0001, 9)) // 3.000000001396984
  }
}
