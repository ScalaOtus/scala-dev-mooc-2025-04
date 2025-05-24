package ru.otus.module1.homework.collections

import scala.collection.immutable.List
import scala.util.Random


// В урне 3 белых и 3 черных шара. Из урны дважды вынимают по одному шару, не возвращая их обратно. Найти вероятность появления белого шара
//
//Как будем делать:
//
//	создать класс с моделированием эксперимента, в нем должна быть коллекция (List) моделирующая урну с шариками (1 - белый шарик, 0 - черный шарик) и функция случайного выбора 2х шариков без возвращения (scala.util.Random), возвращать эта функция должна true (если был выбран белый шар) и false (в противном случае)
//создать коллекцию обьектов этих классов, скажем 10000 элементов, и провести этот эксперимент (функция map)
//посчитать количество элементов массива из пункта 2 где функция вернула true, это количество поделенное на общее количество элементов массива
//PS: чем больше будет количество опытов в пункте 2, тем ближе будет результат моделирования к аналитическому решению
//

object Experiment {

	object Color extends Enumeration {
		type Color = Value
		val White, Black = Value
		def getRandomColor: Color.Value = {
			val randomNumber = scala.util.Random.nextInt(2)
			if (randomNumber == 0) White else Black
		}
	}
	import Color._

	case class Ball(color: Color)
	val whiteBall = Ball(Color.White)
	val blackBall = Ball(Color.Black)

	class Box() {
		private var _storage: List[Ball] = Random.shuffle(
			List(whiteBall, whiteBall, whiteBall, blackBall, blackBall, blackBall)
		)
		private def addBall(ball: Ball): Unit = {
			_storage = ball :: _storage
		}
		def getBall: Ball = _storage match {
			case ::(head, next) => {
				_storage = _storage.tail
				head
			}
			case Nil => throw new Exception("Empty storage!")
		}
		def pick2Balls: List[Ball] = List(getBall, getBall)
	}

	private var _sum = 0.0;

	def main(args: Array[String]): Unit = {
		for (i <- 1 to 10_000) {
			val box = new Box
			_sum += (if (box.pick2Balls.contains(whiteBall)) 1 else 0)
		}
		println(_sum / 10_000) // 0.80
	}

}
