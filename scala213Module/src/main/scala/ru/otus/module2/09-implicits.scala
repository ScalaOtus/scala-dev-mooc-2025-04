package ru.otus.module2

object implicits {

  // implicit conversions

  object implicit_conversions {

    /** Расширить возможности типа String, методом trimToOption, который возвращает Option[String]
      * если строка пустая или null, то None
      * если нет, то Some от строки со всеми удаленными начальными и конечными пробелами
      */

      // 1 передаем в качестве аргумента не подходящий тип
      // 2 вызов не существующего метода


    class StringOps(str: String) {
      def trimToOption: Option[String] =
        Option(str).map(_.trim).filter(_.nonEmpty)
    }

    implicit def strToStringOps(str: String): StringOps = new StringOps(str)

    val r: Option[String] = "hello".trimToOption

//    implicit def strToInt(str: String): Int = Integer.parseInt(str)
//
//    "foo" / 42

//    def log(str: String) = println(str)
//
//    val res = log(3)











  }

  object implicit_scopes {

    trait Printable

    trait Printer[T] extends Printable {
      def print(v: T): Unit
    }

    object Printable {
       implicit val v: Printer[Bar] = new Printer[Bar] {
         override def print(v: Bar): Unit = println(s"Implicit from companion object Printable + $v")
       }
    }

    // companion object Printer
    object Printer {
       implicit val v: Printer[Bar] = new Printer[Bar] {
         override def print(v: Bar): Unit = println(s"Implicit from companion object Printer + $v")
       }
    }

    case class Bar()

    case class Foo()


    // companion object Bar
    object Bar {
        implicit val v: Printer[Bar] = new Printer[Bar] {
          override def print(v: Bar): Unit = println(s"Implicit from companion object Bar + $v")
        }
    }

    // some arbitrary object
    object wildcardImplicits {
      implicit val v: Printer[Bar] = new Printer[Bar] {
        override def print(v: Bar): Unit = println(s"Implicit from wildcard import + $v")
      }
    }

    def print[T](b: T)(implicit m: Printer[T]) = m.print(b)

//     implicit val v1 = new Printer[Bar]{
//       def print(v: Bar): Unit = println(s"Implicit from local val + $v")
//     }

    import wildcardImplicits._

    print(Bar())




  }


}
