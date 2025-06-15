package ru.otus.module1

import ru.otus.module1.opt.{Animal, Cat, Dog}

import scala.annotation.tailrec
import scala.language.postfixOps



/**
 * referential transparency
 */




 // recursion

object recursion {

  /**
   * Реализовать метод вычисления n!
   * n! = 1 * 2 * ... n
   */

  def fact(n: Int): Int = {
    var _n = 1
    var i = 2
    while (i <= n){
      _n *= i
      i += 1
    }
    _n
  }


  def factRec(n: Int): Int = if(n <= 0) 1 else n * factRec(n - 1)


  def factTailRec(n: Int, accum: Int): Int = {
    @tailrec
    def loop(n: Int, accum: Int): Int =
      if(n <= 0) accum
      else loop(n - 1, n * accum)

    loop(n, 1)
  }





  /**
   * реализовать вычисление N числа Фибоначчи
   * F0 = 0, F1 = 1, Fn = Fn-1 + Fn - 2
   */


}



object hof{

  def dumb(string: String): Unit = {
    Thread.sleep(1000)
    println(string)
  }

  // обертки

  def logRunningTime[A, B](f: A => B): A => B = a => {
      val start = System.currentTimeMillis()
      val result = f(a)
      val end = System.currentTimeMillis()
      println(s"Running time: ${end - start}")
      result
  }



  // изменение поведения ф-ции

  def isOdd(i: Int): Boolean = i % 2 > 0
  def not[A](f: A => Boolean): A => Boolean = a => !f(a)
  val isEven: Int => Boolean = not(isOdd)

  isOdd(5) // true
  isEven(5) // false


  // изменение самой функции

  def sum(x: Int, y: Int): Int = x + y

  def partial[A, B, C](a: A)(f: (A, B) => C): B => C =
    f.curried(a)


















}






/**
 *  Реализуем тип Option
 */



 object opt {



  trait Animal
  case class Cat() extends Animal
  case class Dog() extends Animal

  /**
   *
   * Реализовать структуру данных Option, который будет указывать на присутствие либо отсутствие результата
   */

  // Invariance
  // + Covariance Если А является подтипом В, то Option[A] является подтипом Option[B]
  // - Contravariance Если А является подтипом В, то Option[A] является супер типом Option[B]

    // Function1[-R, +T]
  val f1: String => Unit = ???
  val f2: Any => Unit = ???

  def foo(f: String => Unit) = f("Hello")

  foo(f2)
  foo(f1)


  sealed trait Option[+T] {
    def isEmpty: Boolean = if(this.isInstanceOf[None.type]) true else false

    def get: T =  if(this.isInstanceOf[None.type]) throw new Exception("None get")
      else{
        val r = this.asInstanceOf[Some[T]]
        r.v
      }

    def map[B](f: T => B): Option[B] = flatMap(v => Option(f(v)))

    def flatMap[B](f: T => Option[B]): Option[B] = if(isEmpty) None else f(this.get)

    def printIfAny(): Unit = if(isEmpty) None else println(this.get)

    def zip[B](that: Option[B]): Option[(T,B)] = Option(this.get,that.get)

    def filter(f: T => Boolean): Option[T] = if(f(this.get)) Option(this.get) else None
  }

  case class Some[T](v: T) extends Option[T]

  case object None extends Option[Nothing]

  object Option {
    def apply[T](v: T): Option[T] =
      if(v == null) None else Some(v)
  }

  val opt1 : Option[Int] = ???

  val opt2: Option[Option[Int]] = opt1.map(i => Option(i + 1))
  val opt3: Option[Int] = opt1.flatMap(i => Option(i + 1))




  /**
   *
   * Реализовать метод printIfAny, который будет печатать значение, если оно есть
   */


  /**
   *
   * Реализовать метод zip, который будет создавать Option от пары значений из 2-х Option
   */


  /**
   *
   * Реализовать метод filter, который будет возвращать не пустой Option
   * в случае если исходный не пуст и предикат от значения = true
   */

 }

 object list {
   /**
    *
    * Реализовать одно связанный иммутабельный список List
    * Список имеет два случая:
    * Nil - пустой список
    * Cons - непустой, содержит первый элемент (голову) и хвост (оставшийся список)
    */

   def treat(a: Option[Animal]) = ???

    sealed trait List[+T] {

     // prepend
     def ::[TT >: T](elem: TT): List[TT] = new ::(elem, this)

     def mkString(): String = this match {
       case ::(head, tail) => head.toString + " " + tail.mkString()
       case Nil => ""
     }

     def reverse: List[T] = {
       @tailrec
       def loop(lst: List[T], accum: List[T]): List[T] = lst match {
         case Nil => accum
         case ::(head,tail) => loop(tail, new ::(head, accum))
       }
       loop(this,Nil)
     }
     def map[B](f: T => B): List[B] = {
       @tailrec
       def loop(acc: List[B], lst: List[T]): List[B] = lst match {
         case ::(head,tail) => loop(f(head) :: acc, tail)
         case Nil => acc.reverse
       }
       loop(Nil,this)
     }
     def flatMap[B](f: T => List[B]): List[B] = this match {
       case ::(head, tail) => f(head) ++ tail.flatMap(f)
       case Nil => Nil
     }
     def ++[B >: T](otherList: List[B]): List[B] = this match {
       case ::(head, tail) => new::(head, tail ++ otherList)
       case Nil => otherList
     }

     def filter(f: T => Boolean): List[T] = {
       @tailrec
       def loop(lst: List[T], accum: List[T]): List[T] = lst match {
         case ::(head,tail) if f(head) => loop(tail, new ::(head, accum))
         case ::(_,tail) => loop(tail, accum)
         case Nil => accum.reverse
       }
       loop(this,Nil)
     }

   }
    case class ::[T](elem: T, tail: List[T]) extends List[T]
    case object Nil extends List[Nothing]

   object List {
     def apply[A](v: A*): List[A] =
       if(v.isEmpty) Nil
       else ::(v.head, apply(v.tail :_*))
   }

   def incList(lst: List[Int]): List[Int] = {
     lst.map(_ + 1)
   }

   def shoutString(lst: List[String]): List[String] = {
     lst.map(_ + "!")
   }


//   val l1 = List(1, 2, 3)
//   val l1a = 1 :: 2 :: 3 :: Nil
//   val l2: List[Cat] = List(Cat())

   val lst0: List[Int] = 1 :: 2 :: 3 :: 4 :: Nil
   val lstReversd: List[Int] = lst0.reverse
   val lst1: List[Int] = lst0.map(_ * 2)
   val lst4: List[Int] = lst0.filter(_ > 3)
   val lst3 = List("1","2","3").flatMap(elem => List("a" + elem,"b" + elem,"c" + elem))
   val lst5: List[Int] = incList(lst0)
   val lst6: List[String] = shoutString(lst3)






    /**
      * Конструктор, позволяющий создать список из N - го числа аргументов
      * Для этого можно воспользоваться *
      * 
      * Например, вот этот метод принимает некую последовательность аргументов с типом Int и выводит их на печать
      * def printArgs(args: Int*) = args.foreach(println(_))
      */

    /**
      *
      * Реализовать метод reverse который позволит заменить порядок элементов в списке на противоположный
      */

    /**
      *
      * Реализовать метод map для списка который будет применять некую ф-цию к элементам данного списка
      */


    /**
      *
      * Реализовать метод filter для списка который будет фильтровать список по некому условию
      */

    /**
      *
      * Написать функцию incList которая будет принимать список Int и возвращать список,
      * где каждый элемент будет увеличен на 1
      */


    /**
      *
      * Написать функцию shoutString которая будет принимать список String и возвращать список,
      * где к каждому элементу будет добавлен префикс в виде '!'
      */

 }