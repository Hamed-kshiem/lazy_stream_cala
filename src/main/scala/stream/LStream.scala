package stream

abstract sealed trait LStream[+E] {
  def head: E = ???
  def tail: LStream[E] = ???

  def map[R](fn: E => R): LStream[R] = this match {
    case LCons(h, t) => LCons(() => fn(h()), () => t().map(fn))
    case Nomore => Nomore
  }

  def filter(pred: E => Boolean): LStream[E] = this match {
    case LCons(h, t) =>
      val head = h()
      if(pred(head)) LCons(() => head, () => t().filter(pred))
      else t().filter(pred)
    case Nomore => Nomore
  }

  def take(n: Int): LStream[E] = this match {
    case LCons(h, t) if n > 1 => LCons(h, () => t().take(n-1))
    case LCons(h, _) if n == 1 => LCons(h, () => Nomore)
    case Nomore => Nomore
  }

  def foreach(action: E => Unit): Unit = this match {
    case LCons(h, t) => action(h()); t().foreach(action)
    case Nomore => ()
  }

  def toList : List[E] = this match {
    case LCons(h, t) => h() :: t().toList
    case Nomore => Nil
  }

  def find(pred: E => Boolean): Option[E] = this match {
    case LCons(h, t) =>
      val head = h()
      if(pred(head)) Some(head)
      else t().find(pred)
    case Nomore => None
  }

}
case object Nomore extends LStream[Nothing]

case class LCons[E](h: () => E, t: () => LStream[E]) extends LStream[E] {
  override def head: E = h()
  override def tail: LStream[E] = t()
}
// TODO: Nomore and LCons

object LStream {
  def apply[E](head: => E, tail: => LStream[E]) : LStream[E] = LCons(() => head, () => tail)
  def iterate[E](seed: E, next: E => E) : LStream[E] = LCons(() => seed, () => iterate(next(seed), next))
  def numsFrom(n: Int): LStream[Int] = iterate(n, _ + 1)
  def fibs: LStream[Int] = {
    def fibsHelper(a: Int, b: Int): LStream[Int] = LCons(() => a, () => fibsHelper(b, a + b))
    fibsHelper(0, 1)
  }
}

def isPrime(n: Int): Boolean =
  (n > 1) && !(2 to scala.math.sqrt(n).toInt).exists(x => n % x == 0)


object Main {
  def main(args: Array[String]): Unit = {
/*    val primes = LStream.numsFrom(2).filter(isPrime)
    println(primes.take(10).toList)
    println(primes.find(_ > 1000))
    println(primes.find(_ > 10000))
    println(primes.map(_ * 2).take(10).toList)*/
val primes = LStream.numsFrom(2).filter(isPrime)
    println(primes.take(5).toList) // [2, 3, 5, 7, 11]
    val primeSquares = primes.map(x => x * x)
    println(primeSquares.take(5).toList) // [4, 9, 25, 49, 121]
    val primeSquareRoots = primeSquares.map(math.sqrt(_)).take(5)
    println(primeSquareRoots.toList) // [2.0, 3.0, 5.0, 7.0, 11.0]
    primeSquareRoots.foreach(println) // prints 2.0, 3.0, 5.0, 7.0, 11.0
    val firstPrimeGreaterThan100 = primes.find(x => x > 100)
    println(firstPrimeGreaterThan100) // Some(101)
    //____________________________________________________
    println("_____________________example 2 test_______________________________")
    //____________________________________________________
    // Fibonacci numbers stream
    val fibonacci = LStream.iterate((0, 1), x => (x._2, x._1 + x._2))
    val fibonacciNumbers = fibonacci.map(_._1)
    //first 10 elements of the fibonacci stream and convert it to a List
    println(fibonacciNumbers.take(10).toList) // [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]
    val evenFibonacci = fibonacciNumbers.filter(_ % 2 == 0)
    println(evenFibonacci.take(5).toList) // [0, 2, 8, 34, 144]
    val fibonacciSum = evenFibonacci.take(10).toList.sum
    println(fibonacciSum) // 188
    println(fibonacciSum) // 4613732
  }

}