package monoid

import testing.{Gen, Prop}
import testing.Prop._

/**
  * Created by ariwaranosai on 16/8/22.
  *
  */

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {
  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    def zero:String = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    def zero: List[A] = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2
    override def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2
    override def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    override def zero: Boolean = true
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    override def zero: Boolean = false
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
    override def zero: Option[A] = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[(A) => A] {
    override def op(a1: (A) => A, a2: (A) => A): (A) => A = a1 compose a2
    override def zero: (A) => A = identity
  }

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
    forAll(for {
      x <- gen
      y <- gen
      z <- gen
    } yield (x, y, z))(p => m.op(p._1, m.op(p._2, p._3)) == m.op(m.op(p._1, p._2), p._3))

    forAll(gen)(p => m.op(m.zero, p) == p && m.op(p, m.zero) == p)
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)
}
