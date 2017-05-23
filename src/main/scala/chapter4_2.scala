object chapter4_2 {
  import chapter4_1._
  import chapter3._

  def easydiff(f: Double => Double, x: Double, h: Double) = (f(x+h) - f(x))/h
  def halve(x: Double) = x/2
  def differentiate(h0: Double, f: Double => Double, x: Double) = map(easydiff(f, x, h0), repeat[Double](halve, h0))


  def main(args: Array[String]): Unit = {

  }
}