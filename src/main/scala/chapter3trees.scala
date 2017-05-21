import chapter3._

object chapter3trees {
  class Tree[T](h: => T, t: => List[Tree[T]]) {
    val label = h
    def subtrees: List[Tree[T]] = t
    override def toString = s"Tree($label, ???)"
    def string = s"Tree($label, ${subtrees.string})"
  }

  object Tree {
    def apply[T](label: T, t: => List[Tree[T]]): Tree[T] = {
      new Tree(label, t)
    }

    def unapply[T](arg: Tree[T]): Option[(T, List[Tree[T]])] = {
      Some(arg.label, arg.subtrees)
    }
  }

  val singeElemTree = Tree(1, Nil)
  val twoElemTree = Tree(1, Cons(Tree(2, Nil), Nil))
  val threeElemTree = Tree(1, Cons(Tree(2, Nil), Cons(Tree(3, Nil), Nil)))

  val testTree =
    Tree(
      1,
      Cons(
        Tree(2, Nil),
        Cons(
          Tree(
          3,
          Cons(Tree(4, Nil), Nil)
        ),
          Nil
        )
      )
    )

  def foldtree[F, G, A](f: (A, G) => F, g: (F, G) => G, a: G, tree: Tree[A]): F = {
    def foldSubtrees(subtrees: List[Tree[A]]): G = subtrees match {
      case Nil => a
      case Cons(h, t) => g(foldtree(f, g, a, h), foldSubtrees(t))
    }

    f(tree.label, foldSubtrees(tree.subtrees))
  }

  def add = (a: Int, b: Int) => a + b
  def sumtree(tree: Tree[Int]) = foldtree[Int, Int, Int](add, add, 0, tree)

  def labels[A](t: Tree[A]): List[A] = {
    foldtree[List[A], List[A], A](Cons(_, _), append, Nil, t)
  }

  def maptree[A, B](t: Tree[A], f: A => B): Tree[B] =
    foldtree(
      (x: A, y: List[Tree[B]]) => Tree(f(x), y),
      (x: Tree[B], y: List[Tree[B]]) => Cons(x, y),
      Nil,
      t
    )

  def main(args: Array[String]): Unit = {
    sumtree(testTree)
    labels(testTree).string

    maptree[Int, Int](testTree, _ * 1000).string
  }

}
