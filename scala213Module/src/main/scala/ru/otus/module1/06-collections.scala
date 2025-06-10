package ru.otus.module1

import scala.annotation.tailrec


object collections2 {


  sealed trait List[+T] {
    def head: T
    def tail: List[T]

    def isEmpty: Boolean = this match {
      case Nil => true
      case Cons(_, _) => false
    }

    def ::[TT >: T](v: TT): List[TT] = Cons(v, this)

    def :::[TT >: T](that: List[TT]): List[TT] = this match {
      case Cons(head, tail) => head :: tail ::: that
      case Nil => that
    }

    def flatMap[B](f: T => List[B]): List[B] = this match {
      case Cons(head, tail) => f(head) ::: tail.flatMap(f)
      case Nil => Nil
    }

    def flatMap2[B](f: T => List[B]): List[B] =
      foldLeft(List.empty[B]){
        case (acc, el) =>
          acc ::: f(el)
      }

    def map[B](f: T => B): List[B] = flatMap(t => Cons(f(t), Nil))


    @tailrec
    final def foldLeft[B](acc: B)(f: (B, T) => B): B = this match {
      case Cons(head, tail) => tail.foldLeft(f(acc, head))(f)
      case Nil => acc
    }

    def take(n: Int): List[T] = {
      val r = foldLeft((0, List.empty[T])){
        case ((i, acc), el) =>
          if(i == n) (i, acc)
          else (i + 1, el :: acc)
      }
      r._2
    }

    def drop(n: Int): List[T] = ???


  }

  case class Cons[T](head: T, tail: List[T]) extends List[T]

  case object Nil extends List[Nothing]{
    def head = throw new NoSuchElementException("Nil.head")
    def tail = throw new UnsupportedOperationException("Nil.tail")
  }

  object List {
    def empty[T]: List[T] = Nil
  }

  val l1 = Cons(1, Cons(2, Cons(3, Nil)))
  val l2 = Cons(4, Cons(5, Cons(6, Nil)))

  val l3: List[(Int, Int)] = for{
    e1 <- l1
    e2 <- l2
  } yield (e1, e2)

  val l4 = l1.flatMap(e1 => l2.map(e2 => (e1, e2)))

}