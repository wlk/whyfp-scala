import scala.language.higherKinds

abstract class List[+T] {
  def string: String // Required to prevent default `toString` evaluation in Scala Worksheets
}
case object Nil extends List[Nothing] {
  override def toString = "Nil"
  override def string = toString
}

class Cons[T](h: => T, t: => List[T]) extends List[T] {
  val head = h
  def tail: List[T] = t
  override def toString = s"Cons($head, ???)"
  override def string = s"Cons($head, ${tail.string})"
}

object Cons {
  def apply[T](h: T, tail: => List[T] ): Cons[T] = {
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
  if(i == 0) Nil
  else Cons(i, f(i-1))
}
f(10).string

def sum(list: List[Int]): Int = list match {
  case Nil => 0
  case Cons(h, tail) => h + sum(tail)
}

sum(emptyList)
sum(singleElem)
sum(twoElem)
sum(threeElem)

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
    list)

sum2(emptyList)
sum2(singleElem)
sum2(twoElem)
sum2(threeElem)

def product(l: List[Int]) = foldr[Int, Int](_ * _, 1, l)

product(emptyList)
product(singleElem)
product(twoElem)
product(threeElem)

def anytrue(l: List[Boolean]) = foldr[Boolean, Boolean](_ || _, false, l)

anytrue(Cons(true, Cons(false, Nil)))
alltrue(Cons(true, Cons(true, Nil)))
anytrue(Cons(false, Cons(false, Nil)))

def alltrue(l: List[Boolean]) = foldr[Boolean, Boolean](_ && _, true, l)

alltrue(Cons(true, Cons(false, Nil)))
alltrue(Cons(true, Cons(true, Nil)))
alltrue(Cons(false, Cons(false, Nil)))

def append[T](a : List[T], b: List[T]): List[T] = foldr[T, List[T]](Cons(_, _), b, a)

append(threeElem, twoElem).string