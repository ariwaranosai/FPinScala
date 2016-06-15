package datastructures

/**
  * Created by ariwaranosai on 16/6/14.
  *
  */

import annotation._

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
    def sum(ints: List[Int]): Int = {
        @tailrec
        def sum_internal(l: List[Int], n: Int): Int = l match {
            case Nil => 0
            case Cons(h, t) => sum_internal(t, h + n)
        }

        sum_internal(ints, 0)
    }

    def product(ds: List[Double]): Double = {
        @tailrec
        def product_internal(l: List[Double], n: Double): Double = l match {
            case Nil => 1
            case Cons(0.0, _) => 0.0
            case Cons(h, t) => product_internal(t, h * n)
        }

        product_internal(ds, 1)
    }

    def tail[A](i: List[A]): List[A] = i match {
        case Nil => throw new IndexOutOfBoundsException()
        case Cons(h, t) => t
    }

    def head[A](i: List[A]): A = i match {
        case Nil => throw new IndexOutOfBoundsException()
        case Cons(h, t) => h
    }

    def setHead[A](h: A, l: List[A]) =
        Cons(h, tail(l))

    @tailrec
    def drop[A](n: Int, l:List[A]): List[A] =
        if (n == 0) l
        else drop(n - 1, tail(l))

    def dropWhile[A](l: List[A])(f: A => Boolean): List[A] =
        if(f(head(l))) dropWhile(tail(l))(f)
        else l

    def append[A](a1: List[A], a2: List[A]): List[A] =
        a1 match {
            case Nil => a2
            case Cons(h, t) => Cons(h, append(t, a2))
        }

    def apply[A](as: A*): List[A] =
        if(as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))

    def init[A](l: List[A]): List[A] = l match {
        case Nil => Nil
        case Cons(h, Nil) => Nil
        case Cons(h, t) => Cons(h, init(t))
    }

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
        as match {
            case Nil => z
            case Cons(h, t) => f(h, foldRight(t, z)(f))
        }

    @tailrec
    def foldLeft[A, B](as :List[A], z: B)(f: (B, A) => B): B =
        as match {
            case Nil => z
            case Cons(h, t) => foldLeft(t, f(z, h))(f)
        }

    def reverse[A](as: List[A]): List[A] =
        foldLeft(as, List[A]())((l, h) => Cons(h, l))

    def foldLeft_r[A, B](as: List[A], z: B)(f: (B, A) => B): B =
        foldRight(as, (b:B) => b) {
            (a,g) => b => g(f(b,a))
        }(z)

    def concat[A](lls: List[List[A]]): List[A] =
        foldLeft(lls, List[A]())(append)

    def length[A](as: List[A]) = foldRight(as, 0)((_, n) => n + 1)

    def sum2(as: List[Int]) = foldRight(as, 0)(_ + _)
    def product2(as: List[Double]) = foldRight(as, 1.0)(_ * _)

    def map[A, B](as: List[A])(f: A => B): List[B] = as match {
        case Nil => Nil
        case Cons(h, t) => Cons(f(h), map(t)(f))
    }

    def map_1[A, B](as: List[A])(f: A => B): List[B] =
        foldRight(as, List[B]())((x, y) => Cons(f(x), y))

    def filter[A](as: List[A])(f: A => Boolean): List[A] =
        foldRight(as, List[A]())((x, y) => if(f(x)) Cons(x, y) else y)

    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
        concat(map(as)(f))

    def filterviaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
        flatMap(as)(x => if(f(x)) List(x) else List())

    def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] =
        (as, bs) match {
            case (Nil, _) => Nil
            case (_, Nil) => Nil
            case (Cons(ha, ta), Cons(hb, tb)) => Cons(f(ha, hb), zipWith(ta, tb)(f))
        }

    @tailrec
    def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l,prefix) match {
        case (_,Nil) => true
        case (Cons(h,t),Cons(h2,t2)) if h == h2 => startsWith(t, t2)
        case _ => false
    }

    @tailrec
    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
        case Nil => sub == Nil
        case _ if startsWith(sup, sub) => true
        case Cons(h,t) => hasSubsequence(t, sub)
    }
}

