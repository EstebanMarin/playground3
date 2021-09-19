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
          //base case
          case Nil => Nil
          //recursive case
          // remove by one the list and by one the n target
          case Cons(_, t) => drop(t, n - 1)

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil else Cons(as.head, apply(as.tail*))

  val ex1: Cons[String] = Cons("a", Cons("b", Nil))
  val ex2: Cons[Int] = Cons(1, Nil)
  val ex3: List[Int] = Cons(2, Nil)
  val ex4: List[Int] = List(1, 2, 3, 4, 5)
  List.sum(ex4)

  val x: Int = List(1, 2, 3, 4, 5) match
    case Cons(x, Cons(2, Cons(4, _)))          => x
    case Nil                                   => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t)                            => h + List.sum(t)
  // case _                                     => 101

  // println(List.tail(List(1, 2, 3)))
  // println(List.tail(List(1)))
  // println(List.setHead(3, List(1, 2, 3)))
  // println(List.setHead("c", List("a", "b")))

  println("─" * 100)
