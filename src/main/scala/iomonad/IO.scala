package iomonad

import scala.io.StdIn._

/**
  * Created by ariwaranosai on 16/8/28.
  *
  */

object IO1 {

  sealed trait IO[A] {
    self =>
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

    def ref[A](a: A): IO[IORef[A]] = IO {
      new IORef(a)
    }

    sealed class IORef[A](var value: A) {
      def set(a: A): IO[A] = IO {
        value = a; a
      }

      def get: IO[A] = IO {
        value
      }

      def modify(f: A => A): IO[A] = get flatMap (a => set(f(a)))
    }

  }

  object Main {

    case class Player(name: String, score: Int)

    def winner(p1: Player, p2: Player): Option[Player] =
      if (p1.score > p2.score) Some(p1)
      else if (p1.score < p2.score) Some(p2)
      else None

    def winnerMsg(p: Option[Player]): String = p map {
      case Player(name, _) => s"$name is the winner!"
    } getOrElse "It's a draw."

    def contest(p1: Player, p2: Player): IO[Unit] =
      PrintLine(winnerMsg(winner(p1, p2)))

    def fahrenheitToCelsius(f: Double): Double = (f - 32) * 5.0 / 9.0

    def ReadLine: IO[String] = IO {
      readLine
    }

    def PrintLine(msg: String): IO[Unit] = IO {
      println(msg)
    }

    def converter: IO[Unit] = for {
      _ <- PrintLine("Enter a temperature in degrees Fahrenheit: ")
      d <- ReadLine map {
        _.toDouble
      }
      _ <- PrintLine(fahrenheitToCelsius(d).toString)
    } yield ()

    val echo = ReadLine.flatMap(PrintLine)
    val readInt = ReadLine.map(_.toInt)
    val readInts: IO[(Int, Int)] = readInt ** readInt

    import IO._

    val helpstring =
      """
        | The Amazing Factorial REPL, v2.0
        | q - quit
        | <number> - compute the factorial of the given number
        | <anything else> - bomb with horrible error
      """.trim.stripMargin

    def factorial(n: Int): IO[Int] = for {
      acc <- ref(1)
      _ <- foreachM(1 to n toStream)(i => acc.modify(_ * i).skip)
      result <- acc.get
    } yield result

    def factorialREPL: IO[Unit] = sequence_(
      IO {
        println(helpstring)
      },
      doWhile {
        IO {
          readLine
        }
      } { line =>
        val ok = line != "q"
        when(ok) {
          for {
            n <- factorial(line.toInt)
            _ <- IO {
              println("factorial: " + n)
            }
          } yield ()
        }
      }
    )

  }

}

object IO2 {
  sealed trait IO[A] {
    def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(this,f)
    def map[B](f: A => B): IO[B] = flatMap(f andThen (Return(_)))
  }

  case class Return[A](a: A) extends IO[A]
  case class Suspend[A](resume: () => A) extends IO[A]
  case class FlatMap[A, B](sub: IO[A], k: A => IO[B]) extends IO[B]

  object IO extends Monad[IO] {
    def apply[A](a: => A): IO[A] = unit(a)
    def unit[A](a: => A): IO[A] = Return(a)
    def flatMap[A, B](a: IO[A])(f: A => IO[B]) = a flatMap f
    def suspend[A](a: => IO[A]) = Suspend(() => ()).flatMap(_ => a)

    def ref[A](a: A): IO[IORef[A]] = IO {
      new IORef(a)
    }

    sealed class IORef[A](var value: A) {
      def set(a: A): IO[A] = IO {
        value = a
        a
      }

      def get: IO[A] = IO { value }

      def modify(f: A => A): IO[A] = for {
        a <- get
        v <- set(f(a))
      } yield v
    }
  }

  def PrintLine(s: String): IO[Unit] =
    Suspend(() => Return(println(s)))

  def ReadLine: IO[String] =
    IO.suspend(Return(readLine()))

  def readInt = ReadLine map (_.toInt)

  def readInts = readInt ** readInt

  import IO._

  def factorial(n: Int): IO[Int] = for {
    acc <- ref(1)
    _ <- foreachM(1 to n toStream)(i => acc.modify(_ * i).skip)
    result <- acc.get
  } yield result


  val helpstring =
    """
      | The Amazing Factorial REPL, v2.0
      | q - quit
      | <number> - compute the factorial of the given number
      | <anything else> - bomb with horrible error
    """.trim.stripMargin

  import IO._

  def factorialREPL: IO[Unit] = sequence_(
    PrintLine(helpstring)
    ,
    doWhile {
      ReadLine
    } { line =>
      val ok = line != "q"
      when(ok) {
        for {
          n <- factorial(line.toInt)
          _ <- PrintLine("factorial: " + n)
        } yield ()
      }
    }
  )

  def readAndAdd: IO[Unit] = for {
    _ <- PrintLine("Input:")
    x <- readInt
    y <- factorial(x)
    _ <- PrintLine(y.toString)
  } yield ()

  @annotation.tailrec
  def run[A](io: IO[A]): A = io match {
    case Return(a) => a
    case Suspend(r) => r()
    case FlatMap(x, f) => x match {
      case Return(a) => run(f(a))
      case Suspend(r) => run(f(r()))
      case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
    }
  }

}

object Main extends App {
  IO2.run(IO2.factorialREPL)
}
