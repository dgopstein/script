#!/usr/bin/scala
!#

case class Val(i: Int) { val add = (_:Int) + i}

case class Def(i: Int) { def add(x: Int) = x + i }

println(Val(0).add.getClass)
println(Def(0).add.getClass)


object implicits { implicit class RichAny[T](self: T) { def let[R](f: T => R) = f(self) } }
object implicits { implicit class RichAny[T](self: T) { def tap[R](f: => R) = {f; self} } }

