package part3

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int =
    ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

  def sum2(ns: List[Int]): Int =
    foldRight(ns, 0)((x,y) => x + y)

  def product(ds: List[Double]): Double =
    ds match {
      case Nil => 1.0
//      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }

  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0)(_ * _)

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](ds: List[A]): List[A] =
    ds match {
      case Cons(_, xs) => xs
      case Nil => sys.error("tail of empty list")
    }

  def setHead[A](ds: List[A], h: A): List[A] =
    ds match {
      case Cons(_, xs) => Cons(h, xs)
      case Nil => sys.error("setHead on empty list")
    }

  def drop[A](l: List[A], n: Int): List[A] = (l, n) match {
    case (as, 0) => as
    case (Cons(x, xs), _n) => drop(xs, _n - 1)
    case (Nil, _) => sys.error("empty list")
  }

  def dropG[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, x) => drop(x, n-1)
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else xs
    case Nil => Nil
  }

  def dropWhile2[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Cons(h, t) if f(h) => dropWhile2(t)(f)
    case _ => as
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def init[A](l: List[A]): List[A] = {
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[A]
    def go(cur: List[A]): List[A] = cur match {
      case Nil => sys.error("nil")
      case Cons(_, Nil) => List(buf.toList: _*)
      case Cons(h, t) => buf += h; go(t)
    }
    go(l)
  }

}

object Exec extends App {
  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case _ => 101
  }
  println(x)

  val xs = List(1,2,3,4,5)
  val ex1 = List.dropWhile2(xs)(x => x < 4)
  println(ex1)
}