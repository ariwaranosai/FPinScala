package state

/**
  * Created by ariwaranosai on 16/7/9.
  *
  */

import State._

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt:(Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)

    val n = (newSeed >>> 16).toInt

    (n, nextRNG)
  }
}

case class State[S, +A](run: S => (A, S)) { self =>

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State((r: S) => {
      val (r1, s1) = self.run(r)
      f(r1).run(s1)
    })

  def map[B](f: A => B): State[S, B] =
    flatMap(x => State.unit(f(x)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(x => sb.map(y => f(x, y)))
}

object State {
  def unit[S, A](a: A): State[S, A] = State[S, A](s => (a, s))
  def sequence[S, A](s: List[State[S, A]]): State[S, List[A]] =
    s.foldRight(unit[S, List[A]](List[A]()))((a, sl) => a.map2(sl)(_ :: _))


  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S) = State[S, Unit](_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    x <- get
    _ <- set(f(x))
  } yield ()

  type Rand[+A] = State[RNG, A]

  def int: Rand[Int] = State[RNG, Int](_.nextInt)
  def double: Rand[Double] = State[RNG, Double](RNG.double)
}

object RNG {
  type State[S, +A] = S => (A, S)
  type Rand[+A] = State[RNG, A]

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)


  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (r, l) = s(rng)
      (f(r), l)
    }

  def map_f[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(s => unit(f(s)))


  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (r1, s1) = ra(rng)
      val (r2, s2) = rb(s1)

      (f(r1, r2), s2)
    }

  def map2_f[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(x => map(rb)(y => f(x, y)))

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  def randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((ra, rl) => map2(ra, rl)(_ :: _))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (r1, s1) = f(rng)
    g(r1)(s1)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt)(i => {
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    })



  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(x => x - x % 2)

  def double_m: Rand[Double] =
    map(nonNegativeInt)(x => x / Int.MaxValue.toDouble + 1)

  def randomPair(rng: RNG):((Int, Int), RNG) = {
    val (r1, s1) = rng.nextInt
    val (r2, s2) = s1.nextInt
    ((r1, r2), s2)
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (r, s) = rng.nextInt
    val r1 = if(r < 0) -(r + 1) else r
    (r1, s)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (r, s) = nonNegativeInt(rng)

    (r.toDouble / (Int.MaxValue.toDouble + 1), s)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, s1) = rng.nextInt
    val (d, s2) = double(s1)

    ((i, d), s2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), s) = intDouble(rng)
    ((d, i), s)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, s1) = double(rng)
    val (d2, s2) = double(s1)
    val (d3, s3) = double(s2)

    ((d1, d2, d3), s3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0)
      (Nil, rng)
    else {
      val (r, s) = rng.nextInt
      val (l, s1)  = ints(count - 1)(s)
      (r::l, s1)
    }
  }

  def roolDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)
}

object RNGTest {
  def main(args: Array[String]) {
    println(RNG.ints(4)(SimpleRNG(12)))
    println(RNG.sequence(List.fill(5)(RNG.roolDie))(SimpleRNG(12))._1)
  }
}

object StateTest {
  def main(args: Array[String]): Unit = {
  }
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {
  def step(input: Input, machine: Machine): Machine = (machine, input) match {
    case (Machine(true, x, y), Coin) if x > 0 => Machine(false, x, y + 1)
    case (Machine(false, x, y), Turn) => Machine(true, x - 1, y)
    case (m@Machine(true, _, _), Turn) => m
    case (m@Machine(false, _, _), Coin) => m
    case (m, _) => m
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs.map(x => (step _).curried(x)).map(State.modify))
    s <- get
  } yield (s.coins, s.candies)

  def simulateMachine_o(inputs: List[Input]): State[Machine, (Int, Int)] = {

    val ss = for {
      input <- inputs
    } yield State.modify((step _).curried(input))

    for {
      _ <- sequence(ss)
      s <- get
    } yield (s.coins, s.candies)
  }

  def main(args: Array[String]) {
    val t = simulateMachine(List[Input](Coin, Turn, Turn, Coin, Turn)).run(Machine(true, 5, 3))
    print(t)
  }
}
