package com.rockthejvm.part2effects

import scala.util.{Failure, Success, Try}

object IOErrorHandling {
  import cats.effect.IO

  // IO: pure, delay, defer
  // create failed effects
  val aFailedCompute: IO[Int] = IO.delay(throw new RuntimeException("A failure"))
  val aFailure: IO[Int] = IO.raiseError(new RuntimeException("a proper failure"))

  // handle exceptions
  val dealWithIt = aFailure.handleErrorWith {
    case _: RuntimeException => IO.delay(println("I'm still here."))
    // add more cases
  }

  // turn into an either
  val effectAsEither: IO[Either[Throwable, Int]] = aFailure.attempt
  // redeem transform the failure and the success in one go
  val resultAsString: IO[String] = aFailure.redeem(ex => s"FAIL: $ex", value => s"Success: $value")
  // redeemWith
  val resultAsEffect: IO[Unit] = aFailure.redeemWith(ex => IO(println(s"FAIL: $ex")), value => IO(println(s"Success: $value")))

  /**
   * Exercises
   */
  // 1 - construct potentially failed IOs from standard data types (Option, Try, Either)
  def option2IO[A](option: Option[A])(ifEmpty: Throwable): IO[A] = option match {
    case Some(value) => IO.delay(value)
    case None => IO.raiseError(ifEmpty)
  }

  def try2IO[A](aTry: Try[A]): IO[A] = aTry match {
    case Success(value) => IO.delay(value)
    case Failure(ex) => IO.raiseError(ex)
  }

  def either2IO[A](anEither: Either[Throwable, A]):IO[A] = anEither match {
    case Right(value) => IO.delay(value)
    case Left(ex) => IO.raiseError(ex)
  }

  // 2 - handleError, handleErrorWith
  def handleError[A](io: IO[A])(handle: Throwable => A): IO[A] =
    io.redeem(handle, a => a)

  def handleErrorWith[A](io: IO[A])(handle: Throwable => IO[A]): IO[A] =
    io.redeemWith(handle, _ => io)

  def main(args: Array[String]): Unit = {
    import cats.effect.unsafe.implicits.global
    resultAsEffect.unsafeRunSync()
  }
}
