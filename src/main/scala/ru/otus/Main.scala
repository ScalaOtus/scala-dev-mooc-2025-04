package ru.otus

import scala.collection.View


object App {
  def main(args: Array[String]): Unit = {


    val list = List(1, 2, 3)

    val lazyList = LazyList(1, 2, 3)

    println(list)
    println(lazyList)

    val r1: View[Int] = list.view.map{ i =>
      println(s"map $i")
      i + 1
    }.filter{ i =>
      println(s"filter $i")
      i % 2 == 0
    }

    val r2 = lazyList.map{ i =>
      println(s"map lazy $i")
      i + 1
    }.filter{ i =>
      println(s"filter lazy $i")
      i % 2 == 0
    }

    val r3 = r1.to(List)

    val r4 = lazyList.zip(list)

    println(r4)


  }
}