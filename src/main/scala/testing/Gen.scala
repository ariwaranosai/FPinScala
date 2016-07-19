package testing

import state._
import State._

/**
  * Created by ariwaranosai on 16/7/18.
  *
  */

case class Gen[+A](sample: State[RNG, A]) { self =>
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen[B](sample.flatMap(x => f(x).sample))

  def map[B](f: A => B): Gen[B] =
    Gen[B](sample.map(f))

  def listOfN(size: Gen[Int]): Gen[List[A]] = for {
    x <- size
    l <- Gen.listOfN(x, this)
  } yield l

  def unsized: SGen[A] = SGen {_ => this}
}

case class SGen[+A](forSize: Int => Gen[A]) {
  def gen(n: Int): Gen[A] = forSize(n)

  def flatMap[B](f: A => SGen[B]): SGen[B] =
    SGen[B](x => forSize(x).flatMap(y => f(y).forSize(x)))

  def map[B](f: A => B): SGen[B] = SGen(x => forSize(x).map(f))
}

object Gen {
  implicit def unsized[A](g: Gen[A]): SGen[A] = SGen(_ => g)
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen[Int](State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive-start)))

  def unit[A](a: => A): Gen[A] = Gen[A](State.unit(a))
  def boolean: Gen[Boolean] = Gen[Boolean](int.map(_ > 0))
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen[List[A]](sequence(List.fill(n)(g.sample)))
  def listOfN[A](g: Gen[A]): SGen[List[A]] = SGen[List[A]](n => listOfN(n, g))

  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => listOfN(n max 1, g))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap(if(_) g1 else g2)
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val p = g1._2.abs / (g1._2.abs + g2._2.abs)
    Gen(double.flatMap(d => if (d < p) g1._1.sample else g2._1.sample))
  }
}


object GenTest {
}