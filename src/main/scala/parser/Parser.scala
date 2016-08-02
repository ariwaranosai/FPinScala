package parser

import testing.{Gen, Prop}
import testing.Prop._

import scala.util.matching.Regex

/**
  * Created by sai on 2016/7/20.
  */


trait Parsers[ParseError, Parser[+_]] { self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A =>
    Parser[String]): ParserOps[String] = ParserOps(f(a))
  def map[A, B](a: Parser[A])(f: A => B): Parser[B] = flatMap(a)(x => unit(f(x)))

  implicit def string(s: String): Parser[String]
  implicit def regex(r: Regex): Parser[String]
  def unit[A](a: A): Parser[A]
  def slice[A](p: Parser[A]): Parser[String]
  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)]
  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]
  def flatMap[A, B](a: Parser[A])(f: A => Parser[B]): Parser[B]

  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))((x, y) => x::y) or succeed(List())

  def succeed[A](a: A): Parser[A] = string("") map (_ => a)

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_::_)


  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeed(List())
    else map2(p, listOfN(n - 1, p))(_::_)

  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    product(p, p2) map f.tupled

  case class ParserOps[A](p: Parser[A]) {
    def ||[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def product[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

  }

}

object Parser {

}

