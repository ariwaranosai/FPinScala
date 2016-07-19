package concurrent

/**
  * Created by sai on 2016/7/11.
  */

import java.util.concurrent._


object Par {
  type Par[A] = ExecutorService => Future[A]

  // return for future
  private case class UnitFuture[A](get: A) extends Future[A] {
    override def isDone = true
    override def get(timeout: Long, unit: TimeUnit)= get
    override def isCancelled: Boolean = false
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false
  }

  // todo timeout
//  private case class TimeOutUnitFuture[A, B, V](a: Future[A], b: Future[B], f: (A, B) => V) extends Future {
//    override def isCancelled: Boolean = a.isCancelled && b.isCancelled
//
//    override def get(): V = {
//      lazy val r1 = a.get
//      lazy val r2 = b.get
//
//      UnitFuture(f(r1, r2)).get
//    }
//
//    override def get(timeout: Long, unit: TimeUnit): V = ???
//
//    override def cancel(mayInterruptIfRunning: Boolean): Boolean = a.cancel(mayInterruptIfRunning) && b.cancel(mayInterruptIfRunning)
//
//    override def isDone: Boolean = a.isDone && b.isDone
//  }

  def unit[A](a: A): Par[A] = es => UnitFuture(a)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def fork[A](a: => Par[A]): Par[A] = es => es.submit(new Callable[A] {
    override def call(): A = a(es).get
  })

  def delay[A](fa: => Par[A]): Par[A] = es => fa(es)

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit())((a, _) => f(a))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def asyncF[A, B](f: A => B): A => Par[B] = (a: A) => lazyUnit(f(a))

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = es => {
    lazy val r1 = a(es)
    lazy val r2 = b(es)

    UnitFuture(f(r1.get, r2.get))
  }

  //def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    //ps.foldRight(unit(List[A]()))((r, tl) => map2(r, tl)(_ :: _))

  def sequence[A](ps: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = {
    if (ps.isEmpty) unit(IndexedSeq[A]())
    else if (ps.length == 1) map(ps.head)(x => IndexedSeq[A](x))
    else {
      val (l, r) = ps.splitAt(ps.length / 2)
      map2(sequence(l), sequence(r))(_ ++ _)
    }
  }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    map(sequence(ps.toIndexedSeq))(_.toList)

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] =
    sequence(ps.map(asyncF(f)))

  def flatten[A](p: Par[Par[A]]): Par[A] = es =>
    run(run(p)(es).get())(es)

  def flatMap[A, B](ps: Par[A])(f: A => Par[B]): Par[B] =
    flatten(map(ps)(f))

  def parFilter[A](ps: List[A])(f: A => Boolean): Par[List[A]] = {
    val p = sequence(ps.map(asyncF(x => if(f(x)) List(x) else List[A]())))
    map(p)(_.flatten)
  }

  def run[A](a: Par[A])(implicit s: ExecutorService): Future[A] = a(s)

  def reduce[A](a: List[A])(f: (A, A) => A, threshold: Int = 2): Par[A] =  {
    if (a.length < threshold)
      lazyUnit(a.reduce(f))
    else  {
      val (l, r) = a.splitAt(a.length / 2)
      map2(reduce(l)(f, threshold), reduce(r)(f, threshold))(f)
    }
  }

  //def equal[A](pa: Par[A], pb: Par[A])(implicit es: ExecutorService): Boolean =
   // pa(es).get == pb(es).get

  def equal[A](pa: Par[A], pb: Par[A]): Par[Boolean] =
    map2(pa, pb)(_ == _)
}

object ParTest {
  import Par._

  implicit val executor = Executors.newSingleThreadExecutor()

  def maxNum(a: List[Int]): Int = {
    run(reduce(a)(_ max _)).get
  }

  def main(args: Array[String]) {
    val t = List.range(12, 22)
    run(parMap(t)(x => println(x))).get

    println(maxNum(t))

    executor.shutdown()
  }
}


