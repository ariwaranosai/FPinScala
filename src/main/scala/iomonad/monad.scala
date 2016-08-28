package iomonad

import language.higherKinds
import language.implicitConversions

/**
  * Created by ariwaranosai on 16/8/27.
  *
  */

trait Functor[F[_]] { self =>
  def map[A, B](a: F[A])(f: A => B): F[B]
}

trait Applicative[F[_]] extends Functor[F] { self =>
  def unit[A](a: => A): F[A]
  def apply[A, B](f: F[A => B])(fa: F[A]): F[B]

  override def map[A, B](a: F[A])(f: (A) => B): F[B] =
    apply(unit[A => B](f))(a)
}

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A, B](a: F[A])(f: A => F[B]): F[B]

  override def apply[A, B](f: F[(A) => B])(fa: F[A]): F[B] =
    flatMap(fa)(x => map(f)(y => y(x)))

  override def map[A, B](a: F[A])(f: (A) => B): F[B] = flatMap(a)(x => unit(f(x)))
  def map2[A, B, C](a: F[A], b: F[B])(f: (A, B) => C): F[C] =
    flatMap(a)(x => map(b)(y => f(x, y)))

  implicit def toMonadic[A](a: F[A]): Monadic[F, A] =
    new Monadic[F, A] {
      val F = Monad.this
      def get = a
    }
}

trait Monadic[F[_], A] {
  val F: Monad[F]
  import F._
  def get: F[A]
}
