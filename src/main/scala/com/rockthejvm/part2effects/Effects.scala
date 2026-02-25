package com.rockthejvm.part2effects

import java.time.{Duration, Instant}
import java.util.{Calendar, Date}
import scala.io.StdIn

object Effects {


  case class MyIO[A](unsafeRun: () => A) {
    def map[B](f: A => B): MyIO[B] =
      MyIO(() => f(unsafeRun()))

    def flatMap[B](f: A => MyIO[B]): MyIO[B] =
      MyIO(() => f(unsafeRun()).unsafeRun())
  }

  val anIO: MyIO[Int] = MyIO(() => {
    println("I'm writing something...")
    42
  })

  /**
   *  Exercises
   *  1. An IO which returns current time of the System
   *  2. An IO which measures the duration of a computation (hint: use ex 1)
   *  3. An IO which prints something to the console
   *  4. An IO something reads from console
   */

  // 1
  def currentTime(): MyIO[Long] = MyIO(() => System.currentTimeMillis())
  // 2
  def mymeasure[A](computation: MyIO[A]): MyIO[Long] =
    MyIO(() => {
      val startTime = currentTime().unsafeRun()
      computation.unsafeRun()
      val endTime = currentTime().unsafeRun()

      startTime - endTime
  })

  // answers:
  val clock: MyIO[Long] = MyIO(() => System.currentTimeMillis())
  def measure[A](computation: MyIO[A]): MyIO[Long] = for {
      startTime <- clock
      _ <- computation
      endTime <- clock
    } yield endTime - startTime
/*
  clock.flatMap(startTime => computation.flatMap(_ => clock.map(endTime => endTime - startTime)))

  clock.map(endTime => endTime - startTime) = MyIO(() => System.currentTimeMillis() - startTime)
  clock.flatMap(startTime => computation.flatMap(_ => MyIO(() => System.currentTimeMillis() - startTime)))

 */

  def testTimeIO(): Unit = {
    val test = measure(MyIO(() => Thread.sleep(1000)))
    println(test.unsafeRun())
  }

  // 3
  def putStrLn(text: String): MyIO[Unit] = MyIO(() => println(text))

  // 4
  val read: MyIO[String] = MyIO(() => StdIn.readLine())

  def testConsole(): Unit = {
    val program: MyIO[Unit] = for {
      line1 <- read
      line2 <- read
      _ <- putStrLn(line1 + line2)
    } yield ()

    program.unsafeRun()
  }

  def main(args: Array[String]): Unit = {
//    val runTime = mymeasure(anIO)
//    val runTime = measure(anIO)
//    println(s"runTime: ${runTime.unsafeRun()}")
//      testTimeIO();
    testConsole()
  }
}
