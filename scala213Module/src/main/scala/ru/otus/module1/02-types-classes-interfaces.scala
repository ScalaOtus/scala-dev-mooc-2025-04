package ru.otus.module1

import java.io.{Closeable, File}
import scala.io.{BufferedSource, Source}
import scala.util.{Try, Using}


object type_system {

  /**
   * Scala type system
   *
   */


  def absurd(v: Nothing) = ???


  // Generics


  lazy val file: File = ???
  lazy val source: BufferedSource = Source.fromFile(file)

  lazy val lines: List[String] = try{
    source.getLines().toList
  } finally {
    source.close()
  }

  def ensureClose[R, T](source: R)(release: R => Any)(f: R => T): T = try{
    f(source)
  } finally release(source)


  lazy val lines2: Unit = ensureClose(source)(s => s.close()){s =>
    val list = s.getLines().toList
    list.foreach(println)
  }





  /**
   *
   * class
   *
   * конструкторы / поля / методы / компаньоны
   */

  class User (var email: String, val password: String = "12345") {

    println("User creation")
    def getPassword: String = password
    def setEmail(_email: String) = email = _email

    // def this(email: String) = this(email,"12345")
  }

  val user: User = User("foo@mail.com", "12345")

  object User {
    def apply(email: String, password: String): User = new User(email, password)
    def from(email: String): User = new User(email, "password")
  }

  user.email = "foo2@mail.com"
  println(user.password)


  /**
   * Задание 1: Создать класс "Прямоугольник"(Rectangle),
   * мы должны иметь возможность создавать прямоугольник с заданной
   * длиной(length) и шириной(width), а также вычислять его периметр и площадь
   *
   */


  /**
   * object
   *
   * 1. Паттерн одиночка
   * 2. Ленивая инициализация
   * 3. Могут быть компаньоны
   */


  /**
   * case class
   *
   */

  case class User2(email: String, password: String)

  val user2 = User2("foo@mail.com", "12345")
  val user3 = User2("foo@mail.com", "12345")


  // создать case класс кредитная карта с двумя полями номер и cvc


  /**
   * case object
   *
   * Используются для создания перечислений или же в качестве сообщений для Акторов
   */


  /**
   * trait
   *
   */


  sealed trait UserService {
    def get(id: String): User

    def insert(user: User): Unit

  }

  trait Updatable {
    def update(user: User): User
  }


  class UserServiceImpl extends UserService with Updatable {
    def get(id: String): User = ???

    def insert(user: User): Unit = ???

    def update(user: User): User = ???
  }

  val us: UserService = new UserServiceImpl
  val upd: Updatable = new UserServiceImpl


  class Foo

  val us2: UserService = new UserService {
    override def get(id: String): User = ???

    override def insert(user: User): Unit = ???
  }

  val foo = new Foo with Updatable {
    override def update(user: User): User = ???
  }




  class A {
    def foo() = "A"
  }

  trait B extends A {
    override def foo() = "B" + super.foo()
  }

  trait C extends B {
    override def foo() = "C" + super.foo()
  }

  trait D extends A {
    override def foo() = "D" + super.foo()
  }

  trait E extends C {
    override def foo(): String = "E" + super.foo()
  }


  // CBDA
  // A -> D -> B -> C
  // CBDA
  val v = new A with D with C with B


  // A -> B -> C -> E -> D
  // DECBA
  val v1 = new A with E with D with C with B


  /**
   * Value classes и Universal traits
   */


}