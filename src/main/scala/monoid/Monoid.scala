package monoid

import testing.{Gen, Prop}
import testing.Prop._

import scala.collection.TraversableOnce.MonadOps

/**
  * Created by ariwaranosai on 16/8/22.
  *
  */

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {
  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    def zero:String = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    def zero: List[A] = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2
    override def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2
    override def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    override def zero: Boolean = true
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    override def zero: Boolean = false
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
    override def zero: Option[A] = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[(A) => A] {
    override def op(a1: (A) => A, a2: (A) => A): (A) => A = a1 compose a2
    override def zero: (A) => A = identity
  }

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
    forAll(for {
      x <- gen
      y <- gen
      z <- gen
    } yield (x, y, z))(p => m.op(p._1, m.op(p._2, p._3)) == m.op(m.op(p._1, p._2), p._3))

    forAll(gen)(p => m.op(m.zero, p) == p && m.op(p, m.zero) == p)
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = as.map(f).foldRight(m.zero)(m.op)

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = foldMap(as, endoMonoid[B])(f.curried)(z)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if(as.isEmpty)
      m.zero
    else if(as.length == 1)
      f(as(0))
    else {
      val (l, r) = as.splitAt(as.length / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }

  import concurrent.NonBlocking._

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    override def op(a1: Par[A], a2: Par[A]): Par[A] = map2(a1, a2) {
      case (x, y) => m.op(x, y)
    }
    override def zero: Par[A] = unit(m.zero)
  }

  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {
    flatMap(parMap(v.toList)(f)) { x =>
      foldMapV(x.toIndexedSeq, par(m))(b => lazyUnit(b))
    }
  }

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lSub: String, words: Int, rStub: String) extends WC

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def op(a1: WC, a2: WC): WC =
      (a1, a2) match {
        case (Stub(x), Stub(y)) => Stub(x + y)
        case (Stub(x), Part(l, n, r)) => Part(x + l, n, r)
        case (Part(l, n, r), Stub(x)) => Part(l, n, r + x)
        case (Part(ll, ln, lr), Part(rl, rn, rr)) =>
          Part(ll, ln + rn + (if((lr + rl).isEmpty) 0 else 1),rr)
      }
    override def zero: WC = Stub("")
  }

  def wordCount(s: String) = {
    def char2WC(c: Char):WC =
      if(c.isWhitespace) Part("", 0, "")
    else
        Stub(c.toString)

    foldMapV(s.toCharArray, wcMonoid)(char2WC) match {
      case Stub(x) => if(x.isEmpty) 0 else 1
      case Part(l, n, r) => (if(l.isEmpty) 0 else 1) + n + (if(r.isEmpty) 0 else 1)
    }
  }

  trait Foldable[F[_]] {
    import Monoid._
    def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
      foldMap(as)(f.curried)(endoMonoid[B])(z)
    def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B
    def foldMap[A, B](as: F[A])(f: A => B)(m: Monoid[B]): B
    def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)
  }

  object ListFoldable extends Foldable[List] {
    override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)
    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)
    override def foldMap[A, B](as: List[A])(f: (A) => B)(m: Monoid[B]): B =
      as.foldRight(m.zero)((x, y) => m.op(f(x), y))
  }

  object IndexedSeqFoldable extends Foldable[IndexedSeq] {
    override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)
    override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)
    override def foldMap[A, B](as: IndexedSeq[A])(f: (A) => B)(m: Monoid[B]): B =
      as.foldRight(m.zero)((x, y) => m.op(f(x), y))
  }

  object StreamFoldable extends Foldable[Stream] {
    override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)
    override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)
    override def foldMap[A, B](as: Stream[A])(f: (A) => B)(m: Monoid[B]): B =
      as.foldRight(m.zero)((x, y) => m.op(f(x), y))
  }

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object TreeFoldable extends Foldable[Tree] {
    override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = {
      as match {
        case Leaf(x) => f(x, z)
        case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
      }
    }

    override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = {
      as match {
        case Leaf(x) => f(z, x)
        case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
      }
    }

    override def foldMap[A, B](as: Tree[A])(f: (A) => B)(m: Monoid[B]): B = as match {
      case Leaf(x) => f(x)
      case Branch(l, r) => m.op(foldMap(l)(f)(m), foldMap(r)(f)(m))
    }
  }

  object OptionFoldable extends Foldable[Option] {
    override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = as match {
      case None => z
      case Some(x) => f(x, z)
    }

    override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = as match {
      case None => z
      case Some(x) => f(z, x)
    }

    override def foldMap[A, B](as: Option[A])(f: (A) => B)(m: Monoid[B]): B = as match {
      case None => m.zero
      case Some(x) => m.op(m.zero, f(x))
    }
  }


  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    override def op(a1: (A, B), a2: (A, B)): (A, B) = (A.op(a1._1, a2._1), B.op(a1._2, a2._2))
    override def zero: (A, B) = (A.zero, B.zero)
  }

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    override def op(a1: Map[K, V], a2: Map[K, V]): Map[K, V] = (a1.keySet ++ a2.keySet).foldLeft(zero) {
      (acc, k) => acc.updated(k, V.op(a1.getOrElse(k, V.zero), a2.getOrElse(k, V.zero)))
    }
    override def zero: Map[K, V] = Map[K, V]()
  }

  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] = new Monoid[(A) => B] {
    override def op(a1: (A) => B, a2: (A) => B): (A) => B =
      (x: A) => B.op(a1(x), a2(x))
    override def zero: (A) => B = (_: A) => B.zero
  }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    foldMapV(as, mapMergeMonoid[A, Int](intAddition))((x: A) => Map(x -> 1))

}
