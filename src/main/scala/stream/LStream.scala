package stream

abstract sealed trait LStream[+E] {
  def head: E = ???
  def tail: LStream[E] = ???
  def map[R](fn: E => R): LStream[R] = ???
  def filter(pred: E => Boolean): LStream[E] = ???
  def take(n: Int): LStream[E] = ???
  def foreach(action: E => Unit): Unit = ???
  def toList : List[E] = ???
  def find(pred: E => Boolean): Option[E] = ???
}

// TODO: Nomore and LCons

object LStream {
  def apply[E](head: => E, tail: => LStream[E]) : LStream[E] = ???
  def iterate[E](seed: E, next: E => E) : LStream[E] = ???
  def numsFrom(n: Int): LStream[Int] = ???
}

def isPrime(n: Int): Boolean =
  (n > 1) && !(2 to scala.math.sqrt(n).toInt).exists(x => n % x == 0)




object Main {
  def main(args: Array[String]): Unit = {

  }
}