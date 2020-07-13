package leftandright

sealed trait LinkedList[+A]

case class Node[A](data: A, next: LinkedList[A]) extends LinkedList[A]

case object End extends LinkedList[Nothing]

object LeftAndRight {
  @annotation.tailrec
  def foldLeft[A, B](as: LinkedList[A], b: B, f: (A, B) => B): B = as match {
    case End => b
    case Node(data, next) => foldLeft(next, f(data, b), f)
  }

  // Note: this implementation is _not_ stack safe
  def foldRight[A, B](as: LinkedList[A], b: B, f: (A, B) => B): B = as match {
    case End => b
    case Node(data, next) => f(data, foldRight(next, b, f))
  }

  def doubleUp[A](as: LinkedList[A]): LinkedList[A] =
    foldRight(as, End, (elem: A, acc: LinkedList[A]) => Node(elem, Node(elem, acc)))

  def reverse[A](as: LinkedList[A]): LinkedList[A] =
    foldLeft(as, End, (elem: A, acc: LinkedList[A]) => Node(elem, acc))

  def foldRightWithReverse[A, B](as: LinkedList[A], b: B, f: (A, B) => B): B =
    foldLeft(reverse(as), b, f)

  def range(from: Int, to: Int): LinkedList[Int] = {
    @annotation.tailrec
    def rec(cur: Int, acc: LinkedList[Int]): LinkedList[Int] =
      if (cur < from) acc
      else rec(cur - 1, Node(cur, acc))

    rec(to, End)
  }

  def map[A, B](as: LinkedList[A], f: A => B): LinkedList[B] =
    foldLeft(reverse(as), End, (a: A, b: LinkedList[B]) => Node(f(a), b))

  def strangeTransform[A, B](as: LinkedList[A], f: (A, B) => B): LinkedList[B => B] =
    map(as, (a: A) => (b: B) => f(a, b))

  def strangeComposition[A, B](as: LinkedList[A], f: (A, B) => B): B => B =
    foldLeft(
      strangeTransform(as, f),
      (b: B) => b,
      (cur: B => B, acc: B => B) => (b: B) => acc(cur(b))
    )

  def strangeFoldRight[A, B](as: LinkedList[A], z: B, f: (A, B) => B): B = {
    strangeComposition(as, f)(z)
  }

  def strangeFoldRightCompact[A, B](as: LinkedList[A], z: B, f: (A, B) => B): B = {
    foldLeft(as,
      (b: B) => b, // Identify function
      (a: A, g: B => B) => (b: B) => g(f(a, b)) // Composition
    )(z)
  }

  def main(args: Array[String]): Unit = {
    val intList = Node(1, Node(2, Node(3, Node(4, Node(5, End)))))
    assert(foldLeft(intList, 0, (a: Int, z: Int) => a + z) == 15)
    assert(foldRight(intList, 0, (a: Int, z: Int) => a + z) == 15)
    assert(foldRightWithReverse(intList, 0, (a: Int, z: Int) => a + z) == 15)
    assert(strangeFoldRight(intList, 0, (a: Int, z: Int) => a + z) == 15)
    assert(strangeFoldRightCompact(intList, 0, (a: Int, z: Int) => a + z) == 15)

    assert(doubleUp(intList) ==
      Node(1, Node(1,
        Node(2, Node(2,
          Node(3, Node(3,
            Node(4, Node(4,
              Node(5, Node(5, End)))))))))))

    assert(reverse(intList) == Node(5, Node(4, Node(3, Node(2, Node(1, End))))))

    // We should be able to construct intList using the range function
    assert(range(1, 5) == intList)

    // Adding 1 to each integer 1..5 should produce 2..6
    assert(map(range(1, 5), (x: Int) => x + 1) == range(2, 6))

    // Fold left is stack safe, and should be able to sum a long with without issue
    assert(
      foldLeft(range(0, 10000), 0, (elem: Int, acc: Int) => elem + acc)
        == (10000 * 10001) / 2
    )

    /*
     * On my machine, this threw a StackOverflowError
     * Mileage may vary; you may need to increase the size of the range
     * to observe the same locally.
    assert(
      foldRight(range(0, 10000), 0, (elem: Int, acc: Int) => elem + acc)
        == (10000 * 10001) / 2
    )
     */

    // This implementation of foldRight should be stack safe
    assert(
      foldRightWithReverse(range(0, 10000), 0, (elem: Int, acc: Int) => elem + acc)
        == (10000 * 10001) / 2
    )
  }
}