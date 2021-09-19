package com.estebanmarin
package playground3

object Main extends App:

  println("─" * 100)

  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List:
    def sum(ints: List[Int]): Int = ints match
      case Nil         => 0
      case Cons(x, xs) => x + sum(xs)

    def product(ds: List[Double]): Double = ds match
      case Nil          => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs)  => x * product(xs)

    def tail[A](x: List[A]): List[A] = x match
      case Nil        => sys.error(message = "Empty list")
      case Cons(_, t) => t

    def setHead[A](head: A, x: List[A]): List[A] = x match
      case Nil        => Cons(head, Nil)
      case Cons(_, t) => Cons(head, t)

    def drop[A](l: List[A], n: Int): List[A] =
      //base case
      if (n <= 0) l
      else
        l match
          //Handle a case outside the list
          case Nil => Nil
          //recursive case
          // remove by one the list and by one the n target
          case Cons(_, t) => drop(t, n - 1)

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil else Cons(as.head, apply(as.tail*))

  println(List.drop(List(1, 2, 3), 1))
  println(List.drop(List(1, 2, 3), 0))
  println(List.drop(Nil, 1))

  println("─" * 100)
