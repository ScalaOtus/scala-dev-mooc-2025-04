package ru.otus.module1


object collections {

    sealed trait ListLike[+A] extends Iterable[A]{ self =>

        override def iterator: Iterator[A] = new Iterator[A] {

            private var coll = self

            override def hasNext: Boolean = coll match {
                case Cons(_, _) => true
                case Nil => false
            }

            override def next(): A = coll match {
                case Cons(h, tail) =>
                    coll = tail
                    h
                case Nil => throw new Exception("Next ob empty iterator")
            }
        }

    }

    case class Cons[A](override val head: A, override val tail: ListLike[A]) extends ListLike[A]

    case object Nil extends ListLike[Nothing]

    object ListLike {
        def apply[A](v: A*): ListLike[A] =
            if(v.isEmpty) Nil else Cons(v.head, apply(v.tail:_*))
    }

    ListLike(1, 2, 3).map(_ + 1)

    val numbers = List(1, 2, 3, 4, 5)

    // удвоить элементы списка numbers

    val r1 = numbers.map(_ * 2)

    // получить список всех чисел
    val nestedList = List(List(1, 2, 3), List(4, 5), List(6, 7))

    val allNumbers: List[Int] = nestedList.flatten

    val words = List("Hello", "World")

    // преобразовать список слов в список букв

    val chars: List[Char] = words.flatMap(_.toList)

    // посчитать общее кол-во символов во всех словах
    val totalChars = chars.size

    val longWords = List("apple", "banana", "kiwi", "strawberry")
    // найти слова длиннее 5 букв

    val r2: List[String] = longWords.filter(_.length > 5)

    val r3: Int = numbers.reduce(_ + _)

    val r4 = numbers.fold(1)(_ * _)

    val r5 = numbers.foldLeft(1)(_ * _)
    val r6 = numbers.foldRight(1)(_ * _)

    val numbersSorted = numbers.sorted
    val numbersSorted2 = numbers.sortWith((a, b) => a < b)

    case class User(name: String, age: Int)
    val users = List(User("a", 32), User("b", 45))
    users.sortBy(_.age)


}
