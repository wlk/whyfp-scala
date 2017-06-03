object chapter4_2 {
  import chapter4_1._

  def easydiff(f: Double => Double, x: Double)(h: Double): Double = (f(x + h) - f(x)) / h

  def halve(x: Double): Double = x / 2

  def differentiate(h0: Double, f: Double => Double, x: Double) = map(easydiff(f, x))(repeat(halve, h0))

  def main(args: Array[String]): Unit = {
    println(within(0.01, differentiate(1, x => x, 2))) // 1.0
    println(within(0.01, differentiate(1, x => 2.0 * x, 2))) // 2.0
    println(within(0.00001, differentiate(1, x => x * x, 2))) // 4.000007629394531
  }
}