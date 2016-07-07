package errorhanding

/**
  * Created by sai on 2016/7/7.
  */
sealed trait Validation[+E, +A] {
  def map[B](f: A => B): Validation[E, B] = this match {
    case Success(x) => Success(f(x))
    case e@Failure(_) => e
  }

  def fold[B](f: A => B, g: Seq[E] => B): B = this match {
    case Success(x) => f(x)
    case Failure(x) => g(x)
  }

  def flatMap[EE >: E, B](f: A => Validation[EE, B]): Validation[EE, B] = this match {
    case Success(x) => f(x)
    case Failure(x) => Failure(x)
  }

  def bitmap[EE, B](f: A => B, g: Seq[E] => Seq[EE]): Validation[EE, B] = this match {
    case Success(x) => Success(f(x))
    case Failure(e) => Failure(g(e))
  }

  def map2[B, EE >: E, C](b: Validation[EE, B])(f: (A, B) => C): Validation[EE, C] = (this, b) match {
    case (Success(x), Success(y)) => Success(f(x, y))
    case (Failure(e), Success(_)) => Failure(e)
    case (Success(_), Failure(e)) => Failure(e)
    case (Failure(e1), Failure(e2)) => Failure(e1 ++ e2)
  }
}
case class Success[+A](value: A)  extends Validation[Nothing, A]
// seq can replace by any semgroup
case class Failure[+E](errors: Seq[E]) extends Validation[E, Nothing]

object Validation {}

object ValidationTest {
  case class Person(name: Name, age: Age)
  case class Name(value: String)
  case class Age(value: Int)

  def mkName(name: String): Validation[String, Name] =
    if (name == "" || name == null) Failure(List("Name is empty."))
    else Success(Name(name))

  def mkAge(age: Int): Validation[String, Age] =
    if (age < 0) Failure(List("age is out of range."))
    else Success(Age(age))

  def mkPerson(name: String, age: Int): Validation[String, Person] =
    mkName(name).map2(mkAge(age))(Person(_, _))

  def main(args: Array[String]) {
    println(mkPerson("", -2))
  }
}
