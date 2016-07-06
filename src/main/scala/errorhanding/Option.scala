package errorhanding

import math._

/**
  * Created by sai on 2016/7/6.
  */
sealed trait Option[+A] {
  def map[B](f: A => B) : Option[B] = this match {
    case Some(x) => Some(f(x))
    case None => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(x) => x
    case None => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this.map(Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] =
    flatMap(x => if(f(x)) Some(x) else None)

}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(x => b.map(y => f(x, y)))
//    for {
//      x <- a
//      y <- b
//    } yield f(x, y)

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some[List[A]](Nil)
    case h::ts => for {
      x <- h
      xs <- sequence(ts)
    } yield x::xs
  }

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None}

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h::ts => for {
      x <- f(h)
      xs <- traverse(ts)(f)
    } yield x::xs
  }
}


object OptionTest {
  import Option._
  def variance(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty)
      None
    else {
      val ex2 = xs.map(x => pow(x, 2)).sum / xs.size
      val e2x = pow(xs.sum / xs.size , 2)
      Some(ex2 - e2x)
    }
  }

  def parseInt(a: List[String]): Option[List[Int]] =
    traverse(a)(x => Try(x.toInt))

  def main(args: Array[String]): Unit = {
    assert(variance(List()) == None)
    assert(parseInt(List("1", "2", "3")) == Some(List(1,2,3)))
    assert(parseInt(List("1", "2", ".")) == None)
  }
}

