package concurrent

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService, Executors}
import java.util.concurrent.atomic.AtomicReference

/**
  * Created by ariwaranosai on 16/7/12.
  *
  */

object NonBlocking {
  sealed trait Future[A] {
    private[concurrent] def apply(a: A => Unit): Unit
  }

  type Par[A] = ExecutorService => Future[A]

  def run[A](es: ExecutorService)(a: Par[A]): A = {
    val ref = new AtomicReference[A]

    val latch = new CountDownLatch(1)

    a(es) {
      x => ref.set(x); latch.countDown()
    }

    latch.await()
    ref.get
  }

  def unit[A](a: A): Par[A] =
    es => new Future[A] {
      override private[concurrent] def apply(ax: (A) => Unit): Unit = ax(a)
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => new Future[A] {
      def apply(ax: A => Unit): Unit =
        eval(es)(a(es)(ax))
    }

  def eval(es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit] { def call = r})

  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] =
    es => new Future[C] {
      def apply(ax: C => Unit): Unit = {
        var ar: Option[A] = None
        var br: Option[B] = None

        val combinator = Actor[Either[A, B]](es) {
          case Left(a) => br match {
            case None => ar = Some(a)
            case Some(b) => eval(es)(ax(f(a, b)))
          }

          case Right(b) => ar match {
            case None => br = Some(b)
            case Some(a) => eval(es)(ax(f(a, b)))
          }
        }

        pa(es) (a => combinator ! Left(a))
        pb(es) (b => combinator ! Right(b))

      }
    }

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    es => new Future[B] {
      def apply(cb: B => Unit): Unit =
        pa(es) { x => eval(es)(cb(f(x)))}
    }

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

  def lazyUnit[A](a: => A): Par[A] = fork[A](unit[A](a))

  def asyncF[A, B](f: A => B): A => Par[B] = (a: A) => lazyUnit(f(a))

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] =
    sequence(ps.map(asyncF[A, B](f)))

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es => new Future[A] {
      def apply(cb: A => Unit): Unit =
        cond(es) { b =>
          if (b) eval(es) { t(es)(cb) }
          else eval(es) { f(es)(cb) }
        }
    }

  def choiceN[A](n: Par[Int])(t: List[Par[A]]): Par[A] =
    es => new Future[A] {
      def apply(cb: A => Unit): Unit =
        n(es) {
          b => eval(es) {t(b)(es)(cb)}
        }
    }

  def chooser[I, A](p: Par[I])(t: I => Par[A]): Par[A] =
    es => new Future[A] {
      def apply(cb: A => Unit): Unit = p(es) {
        a => eval(es) { t(a)(es)(cb)}
      }
    }

  def join[A](p: Par[Par[A]]): Par[A] = es => new Future[A] {
    def apply(cb: A => Unit): Unit =
      p(es) {a => eval(es) {a(es)(cb)}}
  }

  def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] =
    join(map(a)(f))
}

object NoBlockingTest {
  import NonBlocking._

  def main(args: Array[String]) {
    val p = parMap(List.range(1, 100000))(math.sqrt(_))

    val excutor = Executors.newFixedThreadPool(4)
    val x = run(excutor)(p)

    println(x)
    excutor.shutdown()
  }
}
