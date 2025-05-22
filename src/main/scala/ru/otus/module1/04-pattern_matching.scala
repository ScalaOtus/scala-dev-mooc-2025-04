package ru.otus.module1

object pattern_matching{
     // Pattern matching

  /**
   * Матчинг на типы
   */

    val i: Any = List(10)

    i match {
      case v: Int => println("Int type")
      case v: String => println("String type")
      case v: List[Int] => println("List[Int] type")
      case v: List[String] => println("List[String] type")
      case  _ => println("Unknown type")
    }




  /**
   * Структурный матчинг
   */




  sealed trait Animal{

    def whoIam: Unit = this match {
      case Dog(name, _) => println(s"I'm dog $name")
      case Cat(name, _) => println(s"I'm cat $name")
    }
  }


  case class Dog(name: String, age: Int) extends Animal
  class Cat(val name: String, val age: Int) extends Animal

  object Cat {
    def unapply(cat: Cat): Option[(String, Int)] = ???
  }

  val Dog(n, ag) = Dog("bobik", 2)

  object Path {
    def unapplySeq(path: String): Option[Seq[String]] =
      Some(path.split('/').filter(_.nonEmpty))
  }

  val url = "/users/john/profile"

  url match {
    case Path("users", userName, "profile") =>
      println(s"User profile for ${userName}")
    case Path("users", _*) => ???
  }


  /**
   * Матчинг на литерал
   */

  lazy val animal: Animal = ???


  val Bim = "Bim"


  animal match {
    case Dog("Bim", age) => ???
    case Cat(name, age) => ???
    case _ => ???
  }


  /**
   * Матчинг на константу
   */

  animal match {
    case Dog(Bim, age) => ???
    case Cat(name, age) => ???
    case _ => ???
  }


  /**
   * Матчинг с условием (гарды)
   */

  animal match {
    case Dog(name, age) if name == "Bim" => ???
    case Cat(name, age) => ???
  }




  /**
   * "as" паттерн
   */

  def treatCat(cat: Cat) = ???
  def treatDog(dog: Dog) = ???


  def treat(a: Animal) = a match {
    case d : Dog =>
      println(d.name)
      treatDog(d)
    case c @ Cat(name, age) => treatCat(c)
  }



  /**
   * Используя паттерн матчинг напечатать имя и возраст
   */




  final case class Employee(name: String, address: Address)
  final case class Address(val street: String, val number: Int)


  case class Person(name: String, age: Int)



  /**
   * Воспользовавшись паттерн матчингом напечатать номер из поля адрес
   */

  val alex = Employee("Alex", Address("XXX", 221))

  alex match {
    case Employee(_, Address(_, number)) => println(number)
  }




  /**
   * Паттерн матчинг может содержать литералы.
   * Реализовать паттерн матчинг на alex с двумя кейсами.
   * 1. Имя должно соотвествовать Alex
   * 2. Все остальные
   */




  /**
   * Паттерны могут содержать условия. В этом случае case сработает,
   * если и паттерн совпал и условие true.
   * Условия в паттерн матчинге называются гардами.
   */



  /**
   * Реализовать паттерн матчинг на alex с двумя кейсами.
   * 1. Имя должно начинаться с A
   * 2. Все остальные
   */


  /**
   *
   * Мы можем поместить кусок паттерна в переменную использую `as` паттерн,
   * x @ ..., где x это любая переменная.
   * Это переменная может использоваться, как в условии,
   * так и внутри кейса
   */

    trait PaymentMethod
    case object Card extends PaymentMethod
    case object WireTransfer extends PaymentMethod
    case object Cash extends PaymentMethod

    case class Order(paymentMethod: PaymentMethod)

    lazy val order: Order = ???

    lazy val pm: PaymentMethod = ???


    def checkByCard(o: Order) = ???

    def checkOther(o: Order) = ???



  /**
   * Мы можем использовать вертикальную черту `|` для матчинга на альтернативы
   */

   sealed trait A
   case class B(v: Int) extends A
   case class C(v: Int) extends A
   case class D(v: Int) extends A

  val a: A = ???

  a match {
    case B(_) | C(_) => ???
    case D(_) => ???
  }

}