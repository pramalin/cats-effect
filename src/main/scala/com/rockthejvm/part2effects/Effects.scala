package com.rockthejvm.part2effects

import java.time.{Duration, Instant}
import java.util.{Calendar, Date}

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
   */

  // 1
  def currentTime(): MyIO[Instant] =
     MyIO(() => {
       Instant.now()
    })

  // 2
  def measure[A](computation: MyIO[A]): MyIO[Long] =
    MyIO(() => {
      val startTime = currentTime().unsafeRun()
      computation.unsafeRun()
      val endTime = currentTime().unsafeRun()

      Duration.between(startTime, endTime).toMillis();
  })

  def main(args: Array[String]): Unit = {
    val runTime = measure(anIO)
   println(s"runTime: ${runTime.unsafeRun()}")
  }
}
