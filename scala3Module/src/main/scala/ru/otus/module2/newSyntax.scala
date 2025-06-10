package ru.otus.module2


/*
* 1. Синтаксические изменения
* */

object syntaxScala2 {
  private var cond = true

  if(cond) {
    println("true")
  }
    else {
      println("false")
  }

  for(i <- 1 to 5){
    println(i)
  }
}

object syntaxScala3:
  private var cond = true
  if cond then println("true")
  else println("false")

  while cond do
    println("Hi")
  end while

  class Foo(x: Int)
  val foo = Foo(10)
end syntaxScala3


// Top level declaration

def foo(): Unit = println("foo")

// Исполняемый метод с помощью аннотации @main
@main def run(): Unit = println("Hello world!")