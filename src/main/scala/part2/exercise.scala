package part2

object exercise extends App {
  def fib(n: Int): Int = {
    if (n < 0) throw new Exception("aa")
    def go(n: Int): Int = {
      if (n == 0) 0
      else if (n == 1) 1
      else go(n - 1) + go(n - 2)
    }
    go(n)
  }
//  for (i <- (0 to 30)) {
//    println("i = " + fib(i))
//  }

  println(formatResult("test", 100, t => t * 10))

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  def findFirst(ss: Array[String], key: String): Int = {
    def loop(n: Int): Int = {
      if (n >= ss.length) -1
      else if (ss(n) == key) n
      else loop(n + 1)
    }
    loop(0)
  }
  println(findFirst(Array("A","B","C"), "B"))
  def findFirstType[A](as: Array[A], p: A => Boolean): Int = {
    def loop(n: Int): Int = {
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)
    }
    loop(0)
  }
  println(findFirstType(Array("A","B","C"), (A: String) => A == "A"))

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    def loop(n: Int): Boolean = {
      if (n + 1 >= as.length) true
      else if (ordered(as(n), as(n + 1))) loop(n + 1)
      else false
    }
    loop(0)
  }
  def funcOrdered(a: Int, b: Int): Boolean = if (a <= b) true else false

//  println(isSorted(Array(1,2,3,4), funcOrdered))
//  println(isSorted(Array(1,2,4), funcOrdered))
//  println(isSorted(Array(1,2,3,2), funcOrdered))
//  println(isSorted(Array(4,2,3,4), funcOrdered))
//  println(isSorted(Array(1,4,3,4), funcOrdered))

  def partial1[A,B,C](a: A, f: (A,B) => C): B => C = (b: B) => f(a, b)

  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)

  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    a => f(g(a))
}
