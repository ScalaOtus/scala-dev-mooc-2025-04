package ru.otus.module2

/*Intersection types example*/

trait A:
  def a(): Int

trait B:
  def b(): Int

def handle(v: A & B): Unit = println(s" a is ${v.a()} b is ${v.b()}")

/*Union type example*/

def handle2(v: A | B): Unit = v match {
  case v: A => println(v.a())
  case v: B => println(v.b())
}

/*Opaque types example*/
object Logarithms:
  opaque type Log = Double

  def create(d: Double): Log = math.log(d)

  def value(l: Log): Double = math.exp(l)

  def +(x: Log, y: Log): Log = math.exp(x) + math.exp(y)
  
def tryLog() = {
  Logarithms.+(Logarithms.create(10), Logarithms.create(20))
}


// dependent methods and functions
trait Entry {
  type Key;
  val key: Key
}

def extractKey(e: Entry): e.Key = e.key // dependent method

val extractor: (e: Entry) => e.Key = extractKey

// Polymorphic functions types example*/

val reverse: [A] => List[A] => List[A] =
  [A] => (l: List[A]) => l.reverse
  
def runReverse() = reverse(List(1, 2, 3))