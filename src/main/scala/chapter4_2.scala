object chapter4_2 {
  import chapter4_1._

  def easydiff(f: Double => Double, x: Double)(h: Double): Double = (f(x + h) - f(x)) / h

  def halve(x: Double): Double = x / 2

  def differentiate(h0: Double, f: Double => Double, x: Double) = map(easydiff(f, x))(repeat(halve, h0))

  def elimerror(n: Double)(list: Cons[Double]): Cons[Double] = list match {
    case Cons(a, Cons(b, rest)) => Cons(b*Math.pow(2, n)-a / (Math.pow(2, n)-1), elimerror(n)(Cons(b, rest)))
  }

  def order(list: Cons[Double]) = list match {
    case Cons(a, Cons(b, Cons(c, _))) =>
      val result = Math.round(Math.log((a-c)/(b-c)-1))

      if(result.isNaN || result == 0) 1L else result
  }

  def improve(s: Cons[Double]) = elimerror(order(s))(s)

  def differentiate2(eps: Double, h0: Double, f: Double => Double, x: Double) = within(eps, improve(differentiate(h0, f, x)))

  def _super(s: Cons[Double]) = map(second)(repeat(improve, s))

  def second(list: Cons[Double]) = list.tail.head

  def differentiate3(eps: Double, h0: Double, f: Double => Double, x: Double) = within(eps, _super(differentiate(h0, f, x)))

  def main(args: Array[String]): Unit = {
    println(within(0.01, differentiate(1, x => x, 2))) // 1.0
    println(within(0.01, differentiate(1, x => 2.0 * x, 2))) // 2.0
    println(within(0.00001, differentiate(1, x => x * x, 2))) // 4.000007629394531

    println(differentiate2(0.00001, 1, x => x * x, 2)) // 4.0

    println(differentiate3(0.001, 1, x => x * x, 2)) // 4.0
  }
}