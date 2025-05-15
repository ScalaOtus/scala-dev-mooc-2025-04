package ru.otus

import ru.otus.module1.{hof, type_system}
import ru.otus.module1.opt.{Option => Opt}
import ru.otus.module1.list.{incList, lst0, lst1, lst3, lst4, lst5, lst6, lstReversd}


object App {
  def main(args: Array[String]): Unit = {

    // Optoin
    Opt("world").printIfAny()
    Opt("world").map(_.toUpperCase).printIfAny()
    Opt("world").flatMap(elem => Opt(elem.toUpperCase)).printIfAny()
    val opt0: Opt[(String,String)] = Opt("hello") zip Opt("world")
    opt0.printIfAny()
    Opt(2).filter(_ > 1).printIfAny()

     List
    println(lst0.mkString(", "))
    println(lstReversd)
    println(lst1)
    println(lst3)
    println(lst4)
    println(lst5)
    println(lst6)

  }
}