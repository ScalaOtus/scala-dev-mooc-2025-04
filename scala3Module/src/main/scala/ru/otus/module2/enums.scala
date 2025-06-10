package ru.otus.module2

/*
* Enums
* */

enum Color:
  case Red
  case Green
  case Blue

enum Optional[+T]:
  def isEmpty: Boolean = this == None

  def map[B](f: T => B): Optional[B] = this match {
    case Some(value: T) => Some(f(value))
    case None => None
  }
  case Some(value: T) extends Optional[T]
  case None extends Optional[Nothing]

object Optional:
  def apply[T](v: T): Optional[T] =
    if v == null then Optional.None else Optional.Some(v)

@main def runEnums(): Unit = {
  val color: Color = Color.Red


  val redOrdinal = color.ordinal // 1
  println(redOrdinal)
  val green = Color.fromOrdinal(1)
  val green2 = Color.valueOf("Green")
  println(green)
  println(green2)
  Color.values.foreach(println)

  val opt1: Optional[Int] = Optional(2)
  val r1: Optional[Int] = opt1.map(_ + 2)
  println(r1)
}