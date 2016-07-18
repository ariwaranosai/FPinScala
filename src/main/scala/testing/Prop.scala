package testing

/**
  * Created by ariwaranosai on 16/7/18.
  *
  */

import Prop._
import state.RNG
import lazying.Stream

sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  override def isFalsified: Boolean = false
}

case class Falsified(failure: FailedCase,
                     successCount: SuccessCount) extends Result {
  override def isFalsified: Boolean = true
}

case class Prop(run: (TestCast, RNG) => Result) { self =>}

object Prop {
  type TestCast = Int
  type SuccessCount = Int
  type FailedCase = String

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch {case e: Exception => Falsified(buildMsg(a, e), i)}
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
}

object PropTest {
  def main(args: Array[String]): Unit = {
  }
}
