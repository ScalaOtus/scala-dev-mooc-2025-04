package ru.otus.module2

import ru.otus.module2.type_classes.JsValue.{JsNull, JsNumber, JsString}


object type_classes {

  sealed trait JsValue

  object JsValue {
    final case class JsObject(get: Map[String, JsValue]) extends JsValue

    final case class JsString(get: String) extends JsValue

    final case class JsNumber(get: Double) extends JsValue

    final case object JsNull extends JsValue
  }


  trait JsonWriter[T] {
    def write(v: T): JsValue
  }

  object JsonWriter {

    def apply[T](implicit ev: JsonWriter[T]): JsonWriter[T] = ev

    def from[T](f: T => JsValue) = new JsonWriter[T] {
      override def write(v: T): JsValue = f(v)
    }

    implicit val inJsonWriter = from[Int](JsNumber(_))

    implicit val strJsonWriter = from[String](JsString)

    implicit def optToJsValue[T](implicit ev: JsonWriter[T]) =
      from[Option[T]] {
        case Some(value) => ev.write(value)
        case None => JsNull
      }
  }

  implicit class JsonSyntax[T](v: T) {
    def toJson(implicit ev: JsonWriter[T]): JsValue =  ev.write(v)
  }



  def toJson[T : JsonWriter](v: T): JsValue = {
    JsonWriter[T].write(v)
  }


  toJson("fdvvbfbv")
  toJson(10)
  toJson(Option(10))
  toJson(Option("dfbgfgnhg"))

  "bghbbgfrbgbngf".toJson
  Option(10).toJson





  // 1 type constructor
  trait Ordering[T] {
    def less(a: T, b: T): Boolean
  }


  object Ordering {
    def from[A](f: (A, A) => Boolean): Ordering[A] = new Ordering[A] {
      override def less(a: A, b: A): Boolean = f(a, b)
    }
    // 2 implicit value
    implicit val intOrdering = from[Int]((a, b) => a < b)

    implicit val strOrdering = from[String]((a, b) => a < b)
  }


  case class User(name: String, age: Int)

  object User {
    implicit val userOrdering: Ordering[User] = Ordering.from[User](_.age < _.age)
  }


  // 3 implicit parameter
  def greatest[A](a: A, b: A)(implicit ordering: Ordering[A]): A =
    if(ordering.less(a, b)) b else a


  greatest(5, 10)
  greatest("ab", "abcd")
  greatest(User("alex", 20), User("bob", 30))


  // 1
  trait Eq[T] {
    def ===(a: T, b: T): Boolean
  }

  // 2
  object Eq {

    def from[T](f: (T, T) => Boolean): Eq[T] = new Eq[T] {
      override def ===(a: T, b: T): Boolean = f(a, b)
    }
    implicit val strEq = from[String](_ == _)
  }

  // 3
  def ===[T](a: T, b: T)(implicit eq: Eq[T]): Boolean = eq.===(a, b)

  // 4 implicit class
  implicit class EqSyntax[T](a: T) {
    def ===(b: T): Boolean = a == b
  }

  val result = List("a", "b", "1").filter(str => str === "a")




}
















