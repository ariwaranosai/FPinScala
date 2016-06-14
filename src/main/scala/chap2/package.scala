/**
  * Created by ariwaranosai on 16/6/13.
  *
  */

package object chap2 {
    def fib(n: Int): Int = {
        @annotation.tailrec
        def fib_internal(i: Int, lastOne: Int, lastTwo: Int): Int = i match {
            case x if x == n => lastTwo + lastOne
            case 1 => fib_internal(i + 1, 0, lastOne)
            case 2 => fib_internal(i + 1, 1, 0)
            case x =>
                fib_internal(i + 1, lastOne + lastTwo, lastOne)
        }

        if (n < 1)
            throw new Exception("number must greater than 0")
        else if(n == 1) 0
        else if (n == 2) 1
        else fib_internal(3, 1, 0)
    }

    def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
        @annotation.tailrec
        def isSorted_internal(index: Int): Boolean = {
            if(as.length - index > 2)
                if (ordered(as(index), as(index + 1)))
                    isSorted_internal(index + 1)
                else false
            else true
        }
        isSorted_internal(0)
    }

    def curry[A, B, C](f: (A, B) => C): A => (B => C) =
        x => y => f(x, y)

    def uncurry[A, B, C](f: A => (B => C)): (A, B) => C =
        (x, y) => f(x)(y)

    def compose[A, B, C](f: B => C, g: A => B): A => C =
        x => f(g(x))
}
