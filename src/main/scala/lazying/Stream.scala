package lazying


 /**
  * Created by ariwaranosai on 16/7/7.
  *
  */

import Stream._

import scala.annotation.tailrec

sealed trait Stream[+A] { self =>
   def headOption: Option[A] = this match {
     case Empty => None
     case Cons(h, t) => Some(h())
   }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }


   def toList: List[A] = this match {
     case Empty => Nil
     case Cons(h, tail) => h() :: tail().toList
   }

   def take(n: Int): Stream[A] = this match {
     case Empty => empty
     case Cons(h, tail) if n == 0 => empty
     case Cons(h, tail) if n >= 1 => cons(h(), tail().take(n - 1))
   }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((x, y) => p(x) || y)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((x, y) => p(x) && y)

  def takeWhile_foldR(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((x, y) => if(p(x)) cons(x, y) else empty)

  @tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, tail) if n == 0 => this
    case Cons(h, tail) if n > 0 => tail().drop(n - 1)
  }

  def headOption_foldR: Option[A] =
    foldRight[Option[A]](None)((x, y) => Some(x))

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => empty
    case Cons(h, tail) if !p(h()) => empty
    case Cons(h, tail) => cons(h(), tail().takeWhile(p))
  }

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((x, y) => f(x) >:: y)

  def map_fold[B](f: A => B): Stream[B] =
    unfold(self) {
      case Cons(h, tl) => Some((f(h()), tl()))
      case _ => None
    }

  def filter[B](f: A => Boolean): Stream[A] =
    foldRight(empty[A])((x, y) => if(f(x)) x >:: y else y)

  def append[B >: A](b: => Stream[B]): Stream[B] =
    foldRight(b)(_ >:: _)

  def +++ [B >: A](b: => Stream[B]): Stream[B] =
    append(b)

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((x, y) => f(x) +++ y)

  def find(f: A => Boolean): Option[A] =
    filter(f).headOption

  def take_fold(n: Int): Stream[A] =
    unfold((n, self)) {
      case (x, Cons(h, tl)) if x > 0 => Some((h(), (x - 1, tl())))
      case _ => None
    }

  def takeWhile_fold(p: A => Boolean): Stream[A] =
    unfold(self) {
      case Cons(h, tl) if p(h()) => Some(h(), tl())
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((self, s2)) {
      case (Cons(h1, tl1), Cons(h2, tl2)) => Some((Some(h1()), Some(h2())), (tl1(), tl2()))
      case (Cons(h1, tl1), _) => Some((Some(h1()), None), (tl1(), empty))
      case (_, Cons(h2, tl2)) => Some((None, Some(h2())), (empty, tl2()))
      case _ => None
    }

  def zip[B](s2: Stream[B]): Stream[(A, B)] =
    self.zipWith(s2)((_, _))

  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] =
    unfold((self, s2)) {
      case (Cons(h1, tl1), Cons(h2, tl2)) => Some((f(h1(), h2()), (tl1(), tl2())))
      case _ => None
    }

  def startWith[B >: A](s: Stream[B]): Boolean =
    zipAll(s).takeWhile((x) => x._2.isDefined).forAll(x => x._1 == x._2)

  def tails: Stream[Stream[A]] =
    unfold(self) {
      case all@Cons(h, tl) => Some((all, tl()))
      case _ => None
    }

  def hasSubsequence[B >: A](s: Stream[B]): Boolean =
    tails exists {_ startWith s}

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((x, y) => (f(x, y._1), f(x, y._1) >:: y._2))._2

}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]


object Stream {

  def cons[A](hd: => A, t1: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = t1
    Cons(() => head, () => tail)
  }

  class ConsWrapper[A](tl: => Stream[A]) {
    def >::[B >: A](h: => B): Stream[B] = cons(h, tl)
  }

  implicit def consWrapper[A](stream: => Stream[A]): ConsWrapper[A] =
    new ConsWrapper(stream)

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = 1 >:: ones

  def constant[A](a: A): Stream[A] = a >:: constant(a)

  def from(n: Int): Stream[Int] = n >:: from(n + 1)

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some(x) => x._1 >:: unfold(x._2)(f)
    case None => empty
  }

  val fibs: Stream[Int] = unfold((0, 1)) { case (x, y) => Some((x, (y, x + y)))}
  val ones_fold = unfold(1)(_ => Some(1, 1))
  def constant_fold[A](a: A) = unfold(null)(_ => Some(a, null))
  def from_fold(a: Int): Stream[Int] = unfold(a)(x => Some((x, x + 1)))

}

object StreamTest {

  def main(args: Array[String]) {
    val t = 1 >:: 2 >:: 3 >:: empty
    assert(t.headOption_foldR.contains(1))
    assert(t.toList == List(1, 2, 3))
    assert(t.take(2).toList == List(1, 2))
    assert(t.drop(2).toList == List(3))
    assert(t.takeWhile(_ < 3).toList == List(1, 2))

    def m(x: Int) = {println(x); x + 1}

    assert(t.filter(_ % 2 == 1).toList == List(1, 3))

    def odd(x: Int): Stream[Int] =
      if (x % 2 == 1) x >:: x >:: empty
      else empty

    assert(t.flatMap(odd).toList == List(1, 1, 3, 3))

    assert(ones.take(4).toList == List(1,1,1,1))

    assert(ones.map_fold(_ + 1).take(2).toList == List(2, 2))

    assert(t.hasSubsequence(from(2).take(2)))
    assert(!t.hasSubsequence(from(2).take(4)))

    assert(t.scanRight[Int](0)(_ + _).toList == List(6, 5, 3, 0))
  }
}
