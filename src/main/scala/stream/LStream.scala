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
    val primes = LStream.numsFrom(2).filter(isPrime)
    println(primes.take(10).toList)
    println(primes.find(_ > 1000))
    println(primes.find(_ > 10000))
    println(primes.map(_ * 2).take(10).toList)


  }
}