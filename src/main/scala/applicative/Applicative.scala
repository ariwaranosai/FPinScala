package applicative

import monad.Functor
import monad.Monad.Id
import monoid.Monoid
import monoid.Monoid.Foldable

/**
  * Created by ariwaranosai on 16/8/27.
  *
  */

trait Applicative[F[_]] extends Functor[F] { self =>
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
  def map2_[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(apply(unit[A => B => C](f.curried))(fa))(fb)

  def unit[A](a: => A): F[A]
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)((f, x) => f(x))


  def map[A, B](fa: F[A])(f: A => B): F[B] = map2(fa, unit(()))((x, y) => f(x))
  def map_[A, B](fa: F[A])(f: A => B): F[B] =
    apply(unit[A => B](f))(fa)

  def map3[A, B, C, D](fa: F[A],
                       fb: F[B],
                       fc: F[C])(f: (A, B, C) => D): F[D] = {
    apply(apply(apply(unit[A => B => C => D](f.curried))(fa))(fb))(fc)
  }

  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] =
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      override def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))
      override def map2[A, B, C](fa: (F[A], G[A]), fb: (F[B], G[B]))(f: (A, B) => C): (F[C], G[C]) =
        (self.map2(fa._1, fb._1)((x, y) => f(x, y)),
          G.map2(fa._2, fb._2)((x, y) => f(x, y)))
    }

  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] =
    new Applicative[({type f[x] = F[G[x]]})#f] {
      override def map2[A, B, C](fa: F[G[A]], fb: F[G[B]])(f: (A, B) => C): F[G[C]] = {
        self.map2(fa, fb)((x, y) => G.map2(x, y)((k, v) => f(k, v)))
      }

      override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))
    }

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((x, xs) => map2(f(x), xs)(_ :: _))

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(identity)

  def sequence[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
    ofa.foldRight(unit(Map[K, V]()))((x, fa) => map2(fa, x._2)((ys, y) => ys.updated(x._1, y)))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    traverse((0 to n).toList)(_ => fa)

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((_, _))
}

sealed trait Validation[+E, +A]
case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]
case class Success[A](a: A) extends Validation[Nothing, A]

object Applicative {
  val streamApplicative = new Applicative[Stream] {
    override def unit[A](a: => A): Stream[A] = Stream.continually(a)
    override def map2[A, B, C](a: Stream[A], b: Stream[B])(f: (A, B) => C): Stream[C] =
      a zip b map f.tupled
    // sequence
    // Stream(1, ?) Stream(2, ?) Stream(3, ?) => Stream(List(1,2,3), ?)
  }

  def validationApplicative[E]: Applicative[({type f[x] = Validation[E, x]})#f]  = new Applicative[({type f[x] = Validation[E, x]})#f] {
    override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] =
      (fa, fb) match {
        case (Success(x), Success(y)) => Success(f(x, y))
        case (Success(_), Failure(h, tl)) => Failure(h, tl)
        case (Failure(h, tl), Success(_)) => Failure(h, tl)
        case (Failure(hx, tlx), Failure(hy, tly)) => Failure(hx, (tlx :+ hy) ++ tly)
    }
    override def unit[A](a: => A): Validation[E, A] = Success(a)
  }

}

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  def join[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(fa => fa)

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)((a: A) => unit(f(a)))

  override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))
}

object Monad {
  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] = new Monad[({type f[x] = Either[E, x]})#f] {
    override def flatMap[A, B](fa: Either[E, A])(f: (A) => Either[E, B]): Either[E, B] =
      fa match {
        case Right(x) => f(x)
        case Left(e) => Left(e)
      }
    override def unit[A](a: => A): Either[E, A] = Right(a)
  }

}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] { self =>
  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))
  def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] =
    traverse(fga)(identity)

  type Id[A] = A
  implicit val idMonad = new Monad[Id] {
    def unit[A](a: => A): Id[A] = a
    override def flatMap[A, B](fa: Id[A])(f: (A) => Id[B]): Id[B] = f(fa)
  }

  override def map[A, B](fa: F[A])(f: (A) => B): F[B] =
    traverse[Id, A, B](fa)(x => idMonad.unit(f(x)))

  type Const[M, B] = M

  implicit def monoidApplicative[M](M: Monoid[M]) = new Applicative[({type f[x] = Const[M, x]})#f] {
      override def unit[A](a: => A): Const[M, A] = M.zero
      override def map2[A, B, C](fa: Const[M, A], fb: Const[M, B])(f: (A, B) => C): Const[M, C] =
        M.op(fa, fb)
    }

  override def foldMap[A, B](as: F[A])(f: (A) => B)(m: Monoid[B]): B =
    traverse[({type f[x] = Const[B, x]})#f, A, Nothing](as)(f)(monoidApplicative(m))

}

object Traverse {
}
