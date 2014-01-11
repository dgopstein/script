#!/usr/bin/scala
!#
//http://tmorris.net/posts/2009-12-02-debut-with-a-catamorphism.html

object MyOption {
  def none[A] = new MyOption[A] {
    def cata[X](s: A => X, n: => X) = n
  }

  def some[A](a: A) = new MyOption[A] {
    def cata[X](s: A => X, n: => X) = s(a)
  }
}

trait MyOption[+A] {
  import MyOption._

  // single abstract method
  def cata[X](some: A => X, none: => X): X

  def map[B](f: A => B): MyOption[B] = cata(f andThen some, none)

  def flatMap[B](f: A => MyOption[B]): MyOption[B] = cata(f, none)

  def getOrElse[AA >: A](e: => AA): AA = cata(identity, e)

  def filter(p: A => Boolean): MyOption[A] = cata(x => if (p(x)) some(x) else none, none)

  def foreach(f: A => Unit): Unit = sys.error("todo")

  def isDefined: Boolean = sys.error("todo")

  def isEmpty: Boolean = sys.error("todo")

  // WARNING: not defined for None
  def get: A = sys.error("todo")

  def orElse[AA >: A](o: MyOption[AA]): MyOption[AA] = sys.error("todo")

  def toLeft[X](right: => X): Either[A, X] = sys.error("todo")

  def toRight[X](left: => X): Either[X, A] = sys.error("todo")

  def toList: List[A] = sys.error("todo")

  def iterator: Iterator[A] = sys.error("todo")
}

import MyOption._

some(1).map(_ + "s").map(x => println("1s" == x))
var i = 0
none[Int].map(2 *).map(_ => i += 1)
println(i == 0)

some(1).flatMap(x => some("2").map(_ + x)).map(("21".== _) andThen println)

println(some(1).getOrElse(5) == 1)
println(none[Int].getOrElse(5) == 5)

some(1).filter(1 ==).map(x => println(1.== x))
some(1).filter(1 !=).map(_ => i += 1)
println(i == 0)



