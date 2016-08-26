package monad

import testing.Gen

/**
  * Created by ariwaranosai on 16/8/26.
  *
  */

trait Monad[F[_]] {
  def unit[A](a: A): F[A]
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

  def map[A, B](ma: F[A])(f: A => B): F[B] = flatMap(ma)(x => unit(f(x)))
  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(x => map(mb)(y => f(x, y)))

  /*
  def sequence[A](fa: List[F[A]]): F[List[A]] = fa match {
    case Nil => unit(List[A]())
    case h::tl => flatMap(h)(x => map(sequence(tl))(y => x::y))
  }
  */
  def sequence[A](fa: List[F[A]]): F[List[A]] =
    fa.foldRight(unit(List[A]()))((x, fb) =>
      flatMap(x)(h => map(fb)(tl => h::tl)))

  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
    sequence(la.map(x => f(x)))

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    sequence((0 to n).map(_ => ma).toList)

  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    map(sequence(ms.map(x => f(x))))(x => x.zip(ms).filter(_._1).map(_._2))

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    x => flatMap(f(x))(g)

  def flatMapViaCompose[A, B](fa: F[A])(f: A => F[B]): F[B] =
    compose((_: Unit) => fa, f)()

  def join[A](mma: F[F[A]]): F[A] =
    flatMap(mma)(x => x)

  def flatMapViaJoin[A, B](fa: F[A])(f: A => F[B]): F[B] =
    join(map(fa)(f))

}

object Monad {
  val genMonad = new Monad[Gen] {
    override def unit[A](a: A): Gen[A] = Gen.unit(a)
    override def flatMap[A, B](ma: Gen[A])(f: (A) => Gen[B]): Gen[B] =
      ma flatMap f
  }

  case class Id[A](value: A)

  val idMonad: Monad[Id] = new Monad[Id] {
    override def flatMap[A, B](ma: Id[A])(f: (A) => Id[B]): Id[B] =
      ma match {
        case Id(x) => f(x)
      }
    override def unit[A](a: A): Id[A] = Id(a)
  }
}
