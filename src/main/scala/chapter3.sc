import scala.language.higherKinds

abstract class List[+T]
case object Nil extends List[Nothing] {
  override def toString = "Nil"
}
case class Cons[T](h: T, tail: List[T]) extends List[T]

val emptyList = Nil
val singleElem = Cons(1, Nil)
val twoElem = Cons(1, Cons(2, Nil))
val threeElem = Cons(1, Cons(2, Cons(3, Nil)))

def sum(list: List[Int]): Int = list match {
  case Nil => 0
  case Cons(h, tail) => h + sum(tail)
}

sum(emptyList)
sum(singleElem)
sum(twoElem)
sum(threeElem)

def foldr[T](f: ((T, T) => T), z: T, list: List[T]): T = list match {
  case Nil => z
  case Cons(h, t) => f(h, foldr(f, z, t))
}

def sum2(list: List[Int]) = foldr[Int](_ + _, 0, list)

sum2(emptyList)
sum2(singleElem)
sum2(twoElem)
sum2(threeElem)

def product(l: List[Int]) = foldr[Int](_ * _, 1, l)

product(emptyList)
product(singleElem)
product(twoElem)
product(threeElem)

def anytrue(l: List[Boolean]) = foldr[Boolean](_ || _, false, l)

anytrue(Cons(true, Cons(false, Nil)))
alltrue(Cons(true, Cons(true, Nil)))
anytrue(Cons(false, Cons(false, Nil)))

def alltrue(l: List[Boolean]) = foldr[Boolean](_ && _, true, l)

alltrue(Cons(true, Cons(false, Nil)))
alltrue(Cons(true, Cons(true, Nil)))
alltrue(Cons(false, Cons(false, Nil)))


def appendSingle[T](elem: T, list: List[T]) = Cons(elem, list)

def append[T](a: T, b: T) = {


  foldr((a, b) => null, b, a)
}