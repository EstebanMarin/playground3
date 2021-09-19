package com.estebanmarin
package playground3

import scala.annotation.tailrec

object Main extends App:

  println("─" * 100)

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean =
    @tailrec
    def go(n: Int): Boolean =
      if (n >= as.length - 1) true
      //if any of them is not ordered, cancel the recursive call
      else if (!ordered(as(n), as(n + 1))) false
      else go(n + 1)

    go(0)

  // println(isSorted[Int](as = Array(7, 5, 1, 3), ordered = (a: Int, b: Int) => a > b))
  // println(
  //   isSorted(Array("Scala", "Exercises"), (x: String, y: String) => x.length < y.length)
  // )

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
    (b: B) => f(a, b)

  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)

  // def f(a: Int, b: Int): Int = a + b
  // def g(a: Int)(b: Int): Int = a + b

  // println(curry(f)(1)(1) == f(1, 1))
  // println(curry(f)(1)(1) == g(1)(1))

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  // println(uncurry(g)(1, 1) == g(1)(1))
  // println(uncurry(g)(1, 1) == f(1, 1))

  def f(b: Int): Int = b / 2
  def g(a: Int): Int = a + 2

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))

  def x: (Int, Int) => Boolean = (x: Int, y: Int) => x < y
  def y: Function2[Int, Int, Boolean] = (x: Int, y: Int) => x < y
  def z = new Function2[Int, Int, Boolean] {
    override def apply(a: Int, b: Int): Boolean = a < b
  }

  lazy val test = (f: Function2[Int, Int, Boolean], x: Int, y: Int) => f(x, y)
  lazy val test2 = (f: (Int, Int) => Boolean, x: Int, y: Int) => f(x, y)

  lazy val t3 = List(1, 2, 3)

  println(compose(f, g)(2))
  println(compose(g, f)(2))

  println("─" * 100)
