package iomonad

import scala.io.StdIn._

/**
  * Created by ariwaranosai on 16/8/28.
  *
  */

sealed trait IO[A] { self =>
  def run: A
  def map[B](f: A => B): IO[B] =
    new IO[B] {
      override def run: B = f(self.run)
    }

  def flatMap[B](f: A => IO[B]): IO[B] =
    new IO[B] {
      override def run: B = f(self.run).run
    }
}

object IO extends Monad[IO] {
  override def flatMap[A, B](a: IO[A])(f: (A) => IO[B]): IO[B] =
    a.flatMap(f)

  override def unit[A](a: => A): IO[A] = new IO[A] {
    override def run: A = a
  }

  def apply[A](a: => A): IO[A] = unit(a)
}

object Main {
  case class Player(name: String, score: Int)

  def winner(p1: Player, p2: Player): Option[Player] =
    if(p1.score > p2.score) Some(p1)
    else if(p1.score < p2.score) Some(p2)
    else None

  def winnerMsg(p: Option[Player]): String = p map {
    case Player(name, _) => s"$name is the winner!"
  } getOrElse "It's a draw."

  def contest(p1: Player, p2: Player): IO[Unit] =
    PrintLine(winnerMsg(winner(p1, p2)))

  def fahrenheitToCelsius(f: Double): Double = (f - 32) * 5.0 / 9.0

  def ReadLine: IO[String] = IO { readLine }
  def PrintLine(msg: String): IO[Unit] = IO { println(msg) }
  def converter: IO[Unit] = for {
    _ <- PrintLine("Enter a temperature in degrees Fahrenheit: ")
    d <- ReadLine map {_.toDouble}
    _ <- PrintLine(fahrenheitToCelsius(d).toString)
  } yield ()

  def main(args: Array[String]): Unit = {
  }

}
