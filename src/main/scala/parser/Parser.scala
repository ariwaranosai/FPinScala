package parser

/**
  * Created by sai on 2016/7/20.
  */


trait Parsers[ParseError, Parser[+_]] { self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  def char(c: Char): Parser[Char]
  def or[A](p1: Parser[A], p2: Parser[A]): Parser[A]
  implicit def string(s: String): Parser[String]
  implicit def asStringParser[A](a: A)(implicit f: A =>
    Parser[String]): ParserOps[String] = ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    def ||[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
  }
}

object Parser {

}
