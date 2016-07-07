package errorhanding

/**
  * Created by sai on 2016/7/7.
  */
sealed trait Validation[+E, +A] {
  def map[B]
}
case class Success[+A](value: A)  extends Validation[Nothing, A]
case class Failure[+E](errors: Seq[E]) extends Validation[E, Nothing]
