package ru.otus

import ru.otus.module1.{hof, type_system}
import ru.otus.module1.opt.{Option => Opt}
import ru.otus.module1.list.{List => Lst}
import ru.otus.module1.list.{lst0,lstReversd,lst1}


object App {
  def main(args: Array[String]): Unit = {
    Opt("world").printIfAny()
    Opt("world").map(_.toUpperCase).printIfAny()

    Opt("world").flatMap(elem => Opt(elem.toUpperCase)).printIfAny()
    val opt0: Opt[(String,String)] = Opt("hello") zip Opt("world")
    opt0.printIfAny()
    Opt(2).filter(_ > 1).printIfAny()

    println(lst0)
    println(lstReversd)
    println(lst1)
  }
}