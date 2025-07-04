package ru.otus.module1

import scala.annotation.tailrec
import scala.language.postfixOps


object Main extends App {
  println("запуск")

  println("option")

  val opt1 : Option[Int] = Option(1)

  val opt2: Option[Option[Int]] = opt1.map(i => Option(i + 1))
  val opt3: Option[Int] = opt1.flatMap(i => Option(i + 1))
  val opt4: Unit = opt1.printIfAny()
  val opt5: Option[(Int, Option[Int])] = opt1.zip(opt2)
  val opt6: Option[Int] = opt1.filter(i => i > 1)

  println(s"opt1: $opt1")
  println(s"opt2: $opt2")
  println(s"opt3: $opt3")
  println(s"opt4: $opt4")
  println(s"opt5: $opt5")
  println(s"opt6: $opt6")

  println("list")

  val l1 = List(1, 2, 3)

  val l2: String = List.mkString(l1)
  val l3: List[Int] = l1.reverse
  val l4: List[Int] = l1.map(i => i + 1)
  val l5: List[Int] = l1.flatMap(i => List(i + 1))
  val l6: List[Int] = l1.filter(i => i > 1)
  val l7: List[Int] = l1.incList

  val l1str = List("hello", "world")
  val l8: List[String] = l1str.shoutString

  println(s"l1: $l1")
  println(s"l2: $l2")
  println(s"l3: $l3")
  println(s"l4: $l4")
  println(s"l5: $l5")
  println(s"l6: $l6")
  println(s"l7: $l7")
  println(s"l8: $l8")

  println("завершение")
}

sealed trait Option[+T] {
  def isEmpty: Boolean = this.isInstanceOf[None.type]

  def get: T =  if(this.isInstanceOf[None.type]) throw new Exception("None get")
    else{
      val r = this.asInstanceOf[Some[T]]
      r.v
    }

  def map[B](f: T => B): Option[B] = flatMap(v => Option(f(v)))

  def flatMap[B](f: T => Option[B]): Option[B] = {
    if (this.isEmpty) None
    else f(this.get)
  }

  def printIfAny(): Unit = this match {
    case Some(v) => println(v)
    case None =>
  }

  def zip[TT](that: Option[TT]): Option[(T, TT)] = {
    if (this.isEmpty || that.isEmpty) None
    else Some((this.get, that.get))
  }

  def filter(p: T => Boolean): Option[T] = {
    if (this.isEmpty) None
    else if (p(this.get)) this
    else None
  }
}

case class Some[T](v: T) extends Option[T]

case object None extends Option[Nothing]

object Option {
  def apply[T](v: T): Option[T] =
    if(v == null) None else Some(v)
}

sealed trait List[+T] {
 def head: T
 def tail: List[T]

 def ::[TT >: T](elem: TT): List[TT] = Cons(elem, this)

 override def toString: String = this match {
   case Nil => "Nil"
   case Cons(h, t) => s"$h :: $t"
 }

 def reverse: List[T] = {
   @tailrec
   def loop(acc: List[T], current: List[T]): List[T] = current match {
     case Nil => acc
     case Cons(h, t) =>
       loop(Cons(h, acc), t)
   }
   loop(Nil, this)
 }

  def map[TT](f: T => TT): List[TT] = this match {
    case Nil => Nil
    case Cons(h, t) => Cons(f(h), t.map(f))
  }

  def ++[TT >: T](that: List[TT]): List[TT] = this match {
    case Nil => that
    case Cons(h, t) => Cons(h, t ++ that)
  }

  def flatMap[TT](f: T => List[TT]): List[TT] = this match {
    case Nil => Nil
    case Cons(h, t) => f(h) ++ t.flatMap(f)
  }

  def filter(p: T => Boolean): List[T] = this match {
    case Nil => Nil
    case Cons(h, t) => if(p(h)) Cons(h, t.filter(p)) else t.filter(p)
  }

  def incList: List[Int] = this match {
    case Nil => Nil
    case Cons(h: Int, t) => Cons(h + 1, t.incList)
  }

  def shoutString: List[String] = this match {
    case Nil => Nil
    case Cons(h: String, t) => Cons(h.toUpperCase(), t.shoutString)
  }
}

case class Cons[T](h: T, t: List[T]) extends List[T] {
  override def head: T = h
  override def tail: List[T] = t
}
case object Nil extends List[Nothing] {
  override def head: Nothing = throw new Exception("Nil head")
  override def tail: Nothing = throw new Exception("Nil tail")
}

object List {

  def apply[A](v: A*): List[A] =
    if (v.isEmpty) Nil
    else Cons(v.head, apply(v.tail: _*))

  def mkString[T](list: List[T]): String = list match {
    case Nil => ""
    case Cons(h, t) => h.toString + " " + mkString(t)
  }
}