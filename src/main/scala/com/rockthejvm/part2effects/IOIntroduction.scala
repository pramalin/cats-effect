package com.rockthejvm.part2effects

import cats.effect.IO

import scala.io.StdIn

object IOIntroduction {

  // IO
  val ourFirstIO: IO[Int] = IO.pure(42) // arg that should not side effects.
  val delayedIO: IO[Int] = IO.delay {
    println("I'm producing an integer")
    54
  }

  val aDelayedIO_v2: IO[Int] = IO {
    println("I'm producing an integer") // apply == delay
    54
  }

  // map, flatMap
  val improvedMeaningOfLife = ourFirstIO.map(_ * 2)
  val printedMeaningOfLife = ourFirstIO.flatMap(mol => IO.delay(println(mol)))

  def smallProgram(): IO[Unit] = for {
    line1 <- IO(StdIn.readLine())
    line2 <- IO(StdIn.readLine())
    _ <- IO.delay(println(line1 + line2))
  } yield ()

  // manN - combine IO effects as tuples
  import cats.syntax.apply._
  val combinedMeaningOfLife: IO[Int] = (ourFirstIO, improvedMeaningOfLife).mapN(_ + _)
  def smallProgram_v2(): IO[Unit] =
    (IO(StdIn.readLine()), IO(StdIn.readLine())).mapN(_ + _).map(println)

  /**
   * Exercises
   */
  // 1 - sequence two IOs and take the result of the LAST one
  // hint: use flatMap
  def sequenceTakeLast[A, B](ioa: IO[A], iob: IO[B]): IO[B] =
    ioa.flatMap(_ => iob)

  // 2 - sequence two IOs and take the result of the FIRST one
  def sequenceTakeFirst[A, B](ioa: IO[A], iob: IO[B]): IO[A] =
    ioa.flatMap(a => iob.flatMap(_ => IO.pure(a)))


  // 3 - repeat an IO effect forever
  // hint: use FlatMap + recursion
  def forever[A](io: IO[A]): IO[A] =
    io.flatMap(e => forever(IO.pure(e)))

  // 4 - convert an IO to a different type
  // hint: use map
  def convert[A, B](ioa: IO[A], value: B): IO[B] =
  ioa.map(_ => value)

  // 5 - discard value inside an IO, just return Unit
  def asUnit[A](ioa: IO[A]): IO[Unit] =
    ioa.map(_ => ())

  // 6 - fix stack recursion
  

  def main(args: Array[String]): Unit = {
    import cats.effect.unsafe.implicits.global
    //println(delayedIO.unsafeRunSync())
    smallProgram_v2().unsafeRunSync()
  }
}
