package datastructures

/**
  * Created by ariwaranosai on 16/7/6.
  *
  */

sealed abstract class Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
    def size[A](t: Tree[A]): Int = t match {
        case Leaf(_) => 1
        case Branch(l, r) => size(l) + size(r)
    }

    def maximum(t: Tree[Int]): Int = t match {
        case Leaf(x) => x
        case Branch(l, r) => maximum(l) max maximum(r)
    }

    def depth[A](t: Tree[A]): Int = t match {
        case Leaf(x) => 0
        case Branch(l, r) => (depth(l) max depth(r)) + 1
    }

    def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
        case Leaf(x) => Leaf(f(x))
        case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }

    // 这玩意不就是mapreduce嘛
    def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
        case Leaf(x) => f(x)
        case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }

    def size_f[A](t: Tree[A]): Int = fold(t)(_ => 1)(_ + _)

    def maximum_f(t: Tree[Int]): Int = fold(t)(x => x)(_ max _)

    def depth_f[A](t: Tree[A]): Int = fold(t)(_ => 0)((x, y) => (x max y) + 1)

    def map_f[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(x => Leaf(f(x)): Tree[B])(Branch(_, _))


    def main(args: Array[String]) {
        val node1 = Leaf(1)
        val node2 = Leaf(2)
        val node3 = Leaf(2)

        val b1 = Branch(node1, node2)
        val r = Branch(node3, b1)

        assert(size(r) == 3)
        assert(size_f(r) == 3)
        assert(maximum(r) == 2)
        assert(depth(r) == 2)
    }
}
