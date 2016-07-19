package testing

/**
  * Created by ariwaranosai on 16/7/18.
  *
  */

import java.util.concurrent.Executors

import Prop._
import ParProp._
import Gen._
import state._
import lazying.Stream
import concurrent.Par
import concurrent.Par.Par

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

case object Proved extends Result {
  override def isFalsified: Boolean = false
}

case class Prop(run: (MaxSize, TestCase, RNG) => Result) { self =>
  def &&(p: Prop): Prop = Prop {(max, test, rng) =>
    run(max, test, rng) match {
      case Passed => p.run(max, test, rng)
      case Proved => p.run(max, test, rng)
      case x => x
    }
  }

  def ||(p: Prop): Prop = Prop { (max, test, rng) =>
    run(max, test, rng) match {
      case Falsified(f, _) => p.tag(f).run(max, test, rng)
      case x => x
    }

  }

  def tag(msg: String) = Prop { (max, n, rng) =>
    run(max,n,rng) match {
      case Falsified(e, c) => Falsified(msg + "\n" + e, c)
      case x => x
    }
  }

}

object Prop {
  type TestCase = Int
  type SuccessCount = Int
  type FailedCase = String
  type MaxSize = Int

  def check(p: => Boolean): Prop = Prop {(_, _, _) =>
    if(p) Proved else Falsified("()", 0)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (_, n,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = Prop {
    (max,n,rng) =>
      val casesPerSize = (n - 1) / max + 1
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g.forSize(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, n, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max,n,rng)
  }


  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def run(p: Prop, maxSize: MaxSize = 100,
          testCase: TestCase = 100,
          rng: RNG = SimpleRNG(System.currentTimeMillis())): Unit =
    p.run(maxSize, testCase, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg.")
      case Passed =>
        println(s"+ OK, passed $testCase tests.")
      case Proved =>
        println("+ OK, proved property.")
    }

}

object ParProp {
  val S = weighted(
    choose(1, 4).map(Executors.newFixedThreadPool) -> 0.75,
    unit(Executors.newCachedThreadPool) -> 0.25
  )

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S ** g) { case (s, a) => f(a)(s).get}

  def checkPar(p: => Par[Boolean]): Prop =
    forAllPar(unit(()))(_ => p)
}

object PropTest {
  def main(args: Array[String]): Unit = {
    implicit val executor = Executors.newSingleThreadExecutor()

    val reverseProp = forAll(Gen.choose(0, 1000).listOfN(Gen.choose(10, 20)))(ns => ns.reverse.lastOption == ns.headOption)
    run(reverseProp)

    val smallInt = Gen.choose(-10, 10)

    val maxProp = forAll(listOf1(smallInt)) {
      ns =>
        val max = ns.max
        !ns.exists(_ > max)
    }
    run(maxProp)

    val trivalProp = check(1 == 1)
    run(trivalProp)

    val p3 = checkPar {
      Par.equal(Par.map(Par.unit(1))(_ + 1),
        Par.unit(2))
    }

    run(p3)

  }
}
