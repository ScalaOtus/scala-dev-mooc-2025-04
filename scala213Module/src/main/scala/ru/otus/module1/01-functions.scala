package ru.otus.module1

object functions {


  /**
   * Функции
   */

  val sum2: (Int, Int) => Int = (a, b) => a + b

  // SAM
  trait Printer {
    def print(s: String): Unit
  }

  val printer: Printer = new Printer {
    override def print(s: String): Unit = println(s)
  }

  val sum3: (Int, Int) => Int = sum

  sum3(3, 5) // 8



  /**
   * Реализовать метод sum, которая будет суммировать 2 целых числа и выдавать результат
   */

  def sum(x: Int, y: Int): Int = x + y

  sum(2, 3) // 5
  sum2(2, 3) // 5



  //Currying

  val sumCurried = (sum _).curried

  sum2(3, 5) // 8

  val p1: Int => Int = sumCurried(3)

  p1(5) // 8


  // Partial function

  val divide: PartialFunction[(Int, Int), Int] = new PartialFunction[(Int, Int), Int] {
    override def isDefinedAt(x: (Int, Int)): Boolean = x._2 != 0

    override def apply(v1: (Int, Int)): Int = v1._1 / v1._2
  }

  divide.isDefinedAt(10, 0) // false
  divide.isDefinedAt(10, 2) // true
  divide(10, 2)

  val ll = List((4, 2), (5, 0), (6, 3))

  val r = ll.collect(divide) // List(2, 2)


  // SAM Single Abstract Method


  /**
   *  Задание 1. Написать ф-цию метод isEven, которая будет вычислять является ли число четным
   */



  /**
   * Задание 2. Написать ф-цию метод isOdd, которая будет вычислять является ли число нечетным
   */


  /**
   * Задание 3. Написать ф-цию метод filterEven, которая получает на вход массив чисел и возвращает массив тех из них,
   * которые являются четными
   */



  /**
   * Задание 4. Написать ф-цию метод filterOdd, которая получает на вход массив чисел и возвращает массив тех из них,
   * которые являются нечетными
   */


  /**
   * return statement
   *
   */

  // Currying

}