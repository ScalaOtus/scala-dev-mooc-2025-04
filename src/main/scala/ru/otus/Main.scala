package ru.otus

import scala.collection.View
import ru.otus.module1.homework.collections.Basket


object App {
  def main(args: Array[String]): Unit = {

    val numOfTests = 1000
    val qualityOfWiteBall: Double = (0 until numOfTests)
      .map(_ => if (Basket(List(0,1,0,1,0,1)).testReault) 1 else 0)
      .sum

    val result: Double = qualityOfWiteBall / numOfTests.toDouble

    println(s"result: $result")


  }
}