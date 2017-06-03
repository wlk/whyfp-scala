object chapter4_3 {
  import chapter4_1._

  def easyintegrate(f: Double => Double)(a: Double, b: Double) = (f(a) + f(b)) * (b - a) / 2.0

  def mid(a: Double, b: Double) = (a + b) / 2.0

  def addpair(pair: (Double, Double)) = pair._1 + pair._2

  // A nicer implementation, but causes StackOverflowError
  def zip2clean(l1: => Cons[Double], l2: => Cons[Double]): Cons[(Double, Double)] = (l1, l2) match {
    case (Cons(a, s), Cons(b, t)) => Cons((a, b), zip2(s, t))
  }

  def zip2(l1: => Cons[Double], l2: => Cons[Double]): Cons[(Double, Double)] = l1 match {
    case a: Cons[Double] => l2 match {
      case b: Cons[Double] => Cons((a.head, b.head), zip2(a.tail, b.tail))
    }
  }

  def integrate(f: Double => Double)(a: Double, b: Double): Cons[Double] = Cons(easyintegrate(f)(a, b), map(addpair)(zip2(integrate(f)(a, mid(a, b)), integrate(f)(mid(a, b), b))))

  def integ(f: Double => Double)(a: Double, b: Double, fa: Double, fb: Double): Cons[Double] = {
    val m = mid(a, b)
    val fm = f(m)
    Cons((fa + fb) * (b - a) / 2.0, map(addpair)(zip2(integ(f)(a, m, fa, fm), integ(f)(m, b, fm, fb))))
  }

  def integrate2(f: Double => Double)(a: Double, b: Double): Cons[Double] = integ(f)(a, b, f(a), f(b))

  def i1(f: Double => Double)(a: Double, b: Double) = within(0.001, integrate2(f)(a, b))

  def main(args: Array[String]): Unit = {
    println(i1(x => x * x)(1.0, 2.0)) //2.33349609375
  }
}