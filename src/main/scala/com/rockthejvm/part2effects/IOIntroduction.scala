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

  def sequenceTakeLast_v2[A, B](ioa: IO[A], iob: IO[B]): IO[B] =
    ioa *> iob // andThen

  def sequenceTakeLast_v3[A, B](ioa: IO[A], iob: IO[B]): IO[B] =
    ioa >> iob // lazy iob


  // 2 - sequence two IOs and take the result of the FIRST one
  def mySequenceTakeFirst[A, B](ioa: IO[A], iob: IO[B]): IO[A] =
    ioa.flatMap(a => iob.flatMap(_ => IO.pure(a)))

  def sequenceTakeFirst[A, B](ioa: IO[A], iob: IO[B]): IO[A] =
    ioa.flatMap(a => iob.map(_ => a))

  def sequenceTakeFirst_v2[A, B](ioa: IO[A], iob: IO[B]): IO[A] =
    ioa <* iob


  // 3 - repeat an IO effect forever
  // hint: use FlatMap + recursion
  def myForever[A](io: IO[A]): IO[A] =
    io.flatMap(e => myForever(IO.pure(e)))

  def forever[A](io: IO[A]): IO[A] =
    io.flatMap(_ => myForever(io))

  def forever_v2[A](io: IO[A]): IO[A] =
    io >> forever_v2(io)

  def forever_v3[A](io: IO[A]): IO[A] =
    io *> forever_v2(io)  // stack overflow

  def forever_v4[A](io: IO[A]): IO[A] =
    io.foreverM

  // 4 - convert an IO to a different type
  // hint: use map
  def convert[A, B](ioa: IO[A], value: B): IO[B] =
  ioa.map(_ => value)

  def convert_v2[A, B](ioa: IO[A], value: B): IO[B] =
    ioa.as(value)

  // 5 - discard value inside an IO, just return Unit
  def asUnit[A](ioa: IO[A]): IO[Unit] =
    ioa.map(_ => ())

  def asUnit_v2[A](ioa: IO[A]): IO[Unit] =
    ioa.as(())  // discouraged

  def asUnit_v3[A](ioa: IO[A]): IO[Unit] =
    ioa.void  // discouraged

  // 6 - fix stack recursion
  def sum(n: Int): Int =
    if (n <= 0) 0
    else n + sum(n - 1)

  def sumIO(n: Int): IO[Int] =
    if (n <= 0) IO(0)
    else for {
      lastNumber <- IO(n)
      prevSum <- sumIO(n - 1)
    } yield prevSum + lastNumber

  // 7 (hard) - write a fibonacci IO that does not crash on recursion
  // hints: use recursion, ignore exponential complexity, use flatMap heavily
  def fibonacci(n: Int): IO[BigInt] =
    if (n < 2) IO(1)
    else for {
//      last <- IO(fibonacci(n - 1)).flatMap(x => x)
//      prev <- IO(fibonacci(n - 2)).flatMap(x => x)
      last <- IO.defer(fibonacci(n - 1)) // same as .defer(...).flatten
      prev <- IO.defer(fibonacci(n - 2))
    } yield last + prev

  def main(args: Array[String]): Unit = {
    import cats.effect.unsafe.implicits.global
    //println(delayedIO.unsafeRunSync())
    // smallProgram_v2().unsafeRunSync()
    (1 to 10).foreach(i => println(fibonacci(i).unsafeRunSync()))
  }
}
