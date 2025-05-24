package ru.otus.module1.homework.collections

import scala.util.Random

class Basket(
              val balls: List[Int]
            ) {

  private val getRandomElemFromBasket: List[Int] => Int = lst => lst(Random.nextInt(lst.length))

  private def pickBall(f: List[Int] => Int): (Option[Int],List[Int]) = {
    if (balls.nonEmpty) {
      val randomElement = f(balls)
      val index = balls.indexOf(randomElement)
      (Some(randomElement), balls.take(index) ++ balls.drop(index + 1))
    } else {
      (None, List())
    }
  }

  private def provideTest: (Option[Int],List[Int]) = {
    val firstAttemp = pickBall(getRandomElemFromBasket)
    val secondAttemp = Basket(firstAttemp._2).pickBall(getRandomElemFromBasket)
    secondAttemp
  }

  def testReault: Boolean = {
    if (provideTest._1.get == 1) true else false
  }
}

object Basket {
  def apply(lst: List[Int]): Basket = new Basket(lst) // List(1, 1, 1, 0, 0, 0)
}
