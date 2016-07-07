package errorhanding

/**
  * Created by sai on 2016/7/6.
  */

import scala.{Option => _, Either => _}

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case e@Left(x) => e
    case Right(x) => Right(f(x))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case e@Left(_) => default
    case Right(x) => x
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case e@Left(_) => e
    case Right(x) => f(x)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    map(x => Right(x)).getOrElse(b)

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      x <- this
      y <- b
    } yield f(x, y)
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]


object Either {
  def Try[A](t: => A): Either[Exception, A] =
    try {
      Right(t)
    } catch {case e: Exception => Left(e) }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
    case Nil => Right(Nil)
    case h::ts => for {
      x <- h
      xs <- sequence(ts)
    } yield x::xs
  }

  def traverse[E, A, B](as: List[A])(
                       f: A => Either[E, B]): Either[E, List[B]] = as match {
    case Nil => Right(Nil)
    case h::ts => for {
      x <- f(h)
      xs <- traverse(ts)(f)
    } yield x::xs
  }
}

object EitherTest {
  import Either._

  def main(args: Array[String]) {
    val a = List("1", "2", "3", ".")

    println(traverse(a)(x => Try(x.toInt)))

  }
}
