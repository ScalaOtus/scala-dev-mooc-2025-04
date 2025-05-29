package ru.otus.module1

import ru.otus.module1.utils.NameableThreads

import java.io.File
import java.util.concurrent.{ExecutorService, Executors}
import scala.collection.mutable
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future, Promise}
import scala.io.{BufferedSource, Source}
import scala.util.{Failure, Success, Try}

object concurrency {
  class Thread1 extends Thread{
    override def run(): Unit = {
      println(s"Hello from ${Thread.currentThread().getName}")
    }
  }

  def printRunningTime[T](v: => Unit): Unit = {
    val start = System.currentTimeMillis()
    v
    val end = System.currentTimeMillis()
    println(s"Running time: ${end - start}")
  }

  def getRatesLocation1: ToyFuture[Int] = ToyFuture{
    Thread.sleep(1000)
    println("GetRatesLocation1")
    10
  }(executors.pool1)

  def getRatesLocation2: ToyFuture[Int] = ToyFuture{
    Thread.sleep(2000)
    println("GetRatesLocation2")
    20
  }(executors.pool1)

  def async(f: => Unit): Unit = {
    val t = new Thread{
      override def run(): Unit = f
    }
    t.start()
    t.join()
  }

  def async2[A](f: => A): A = {
    var r: A = null.asInstanceOf[A]
    val t = new Thread{
      override def run(): Unit = r = f
    }
    t.start()
    r
  }


//  trait ToyFuture[T] {
//    def onComplete[U](f: Try[T] => U): Unit
//    def flatMap[B](f: T => ToyFuture[B]): ToyFuture[B]
//    def map[B](f: T => B): ToyFuture[B]
//  }
//
//  trait ToyPromise[T] {
//    def future: ToyFuture[T]
//    def complete(v: T): ToyPromise[T]
//  }





  class ToyFuture[T] private(v: () => T, executor: ExecutorService) {
    private var isCompleted: Boolean = false
    private var r: Try[T] = null.asInstanceOf[Try[T]]
    private val q = mutable.Queue[Try[T] => _]()


    def onComplete[U](f: Try[T] => U): Unit = {
      if(isCompleted) f(r)
      else q.enqueue(f)
    }

    def flatMap[B](f: T => ToyFuture[B]): ToyFuture[B] = ???
    def map[B](f: T => B): ToyFuture[B] = ???

    private def start() = {
      val t = new Runnable{
        override def run(): Unit = {
          r = Try(v())
          isCompleted = true
          while (q.nonEmpty){
            q.dequeue()(r)
          }
        }
      }
      executor.execute(t)
    }
  }

  object ToyFuture {
    def apply[T](f: => T)(executors: ExecutorService): ToyFuture[T] = {
      val future = new ToyFuture[T](() => f, executors)
      future.start()
      future
    }
  }
}

object executors {
  val pool1: ExecutorService =
    Executors.newFixedThreadPool(2, NameableThreads("fixed-pool-1"))
  val pool2: ExecutorService =
    Executors.newCachedThreadPool(NameableThreads("cached-pool-2"))
  val pool3: ExecutorService =
    Executors.newWorkStealingPool(4)
  val pool4: ExecutorService =
    Executors.newSingleThreadExecutor(NameableThreads("singleThread-pool-4"))
}

object try_{

  def readFromFile(): List[String] = {
    val s: BufferedSource = Source.fromFile(new File("ints.txt"))
    val result: List[String] = try {
      s.getLines().toList
    } catch {
      case e =>
        println(e.getMessage)
        Nil
    }finally {
      s.close()
    }

    result
  }

  def readFromFile2(): Try[List[String]] = {
    val s: BufferedSource = Source.fromFile(new File("ints.txt"))
    val r = Try(s.getLines().toList)
    s.close()
    r
  }

  def readFromFile3(): Try[List[String]] = {
    val source: Try[BufferedSource] = Try(Source.fromFile(new File("ints.txt")))
    def result(s: BufferedSource): Try[List[String]] = Try(s.getLines().toList)
//    val rr = for{
//      s <- source
//      r <- result(s)
//    } yield r

    val rr2 = source.flatMap(s => result(s))
    source.foreach(_.close)
    rr2
  }

}

object future{
  // constructors

  val f1: Future[Int] = Future.successful(10) // no concurrency
  val f2: Future[Int] = Future.failed[Int](new Exception("Ooops")) // no concurrency
  val f3 = Future(longRunningComputation)(scala.concurrent.ExecutionContext.global) // concurrency



  // Execution context

  lazy val ec: ExecutionContext = ExecutionContext.fromExecutor(executors.pool1)
  lazy val ec2: ExecutionContext = ExecutionContext.fromExecutor(executors.pool2)
  lazy val ec3: ExecutionContext = ExecutionContext.fromExecutor(executors.pool3)
  lazy val ec4: ExecutionContext = ExecutionContext.fromExecutor(executors.pool4)



  // combinators
  def longRunningComputation: Int = ???



  def printRunningTime[T](v: => Future[T]): Future[T] = {
//    Future.successful(System.currentTimeMillis()).flatMap{ start =>
//      v.flatMap{ r  =>
//        Future.successful(System.currentTimeMillis()).map{ end =>
//           println(s"Running time: ${end - start}")
//           r
//        }(ec)
//      }(ec)
//    }(ec)
    implicit val iec = ec

    for{
      start <- Future.successful(System.currentTimeMillis())
      r <- v
      end <- Future.successful(System.currentTimeMillis())
      _ = println(s"Running time: ${end - start}")
    } yield r
  }

  def getRatesLocation1: Future[Int] = Future{
    Thread.sleep(1000)
    println("GetRatesLocation1")
    10
  }(ec)

  def getRatesLocation2: Future[Int] = Future{
    Thread.sleep(2000)
    println("GetRatesLocation2")
    20
  }(ec)

  val ff1 = getRatesLocation1
  val ff2 = getRatesLocation2
  def rates: Future[Unit] = {
    implicit val iec = ec
    val r: Future[Unit] = for{
      v1 <- ff1
      v2 <- ff2
    } yield println(v1 + v2)

    r
  }



  def action(v: Int): Int = {
    Thread.sleep(1000)
    println(s"Action $v in ${Thread.currentThread().getName}")
    v
  }

  val f01 = Future(action(10))(ec)
  val f02 = Future(action(20))(ec2)

  implicit val iec = ec
  val f03 = f01.flatMap{ v1 =>
    action(50)
    f02.map{ v2 =>
      action(v1 + v2)
    }
  }


  // Execution contexts








}

object promise {


  val p: Promise[Int] = Promise[Int]() // no concurrency
  p.isCompleted // false
  val f1: Future[Int] = p.future // no concurrency
  f1.map(_ + 1)(scala.concurrent.ExecutionContext.global)

  p.complete(Try(10))
  p.isCompleted // true

  def map[T, B](v: Future[T])(f: T => B): Future[B] = {
    val p = Promise[B]()
    v.onComplete {
      case Failure(exception) =>
        p.failure(exception)
      case Success(value) =>
        p.complete(Try(f(value)))
    }(scala.concurrent.ExecutionContext.global)
    p.future
  }
}


