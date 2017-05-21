import chapter3._

object chapter4 {

  def next(n: Double, x: Double) = (x + n/x) / 2

  def repeat[A](f: A => A, a: A): Cons[A] = Cons(a, repeat(f, f(a)))

  def main(args: Array[String]): Unit = {

  }
}
