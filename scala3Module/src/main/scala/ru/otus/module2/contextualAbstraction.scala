package ru.otus.module2



extension (s: String)
  def trimToOption: Option[String] =
    Option(s).map(_.trim).filter(_.nonEmpty)

val strOpt = " ".trimToOption

trait Ordering[T]:
  def less(a: T, b: T): Boolean

def max[T](a: T, b: T)(using  ord: Ordering[T]): T =
  if ord.less(a, b) then b else a



object instances {
  given Ordering[Int] with
    def less(a: Int, b: Int): Boolean = a < b
}

object PostConditions:
  opaque type Wrap[T] = T
  type Condition[T] = Wrap[T] ?=> Boolean // (using T) => Boolean

  def result[T](using t: Wrap[T]): T = t

  extension [T](x : T)
    def ensuring(cond: Wrap[T] ?=> Boolean): T = {
      assert(cond(using x))
      x
    }

def runContexts() = {
//  import instances.given
//  max(10, 5)

  import ru.otus.module2.PostConditions.{result, ensuring}

  List(1, 2, 3).sum.ensuring(result == 6)  // Wrap[T] ?=> Boolean == 6
}