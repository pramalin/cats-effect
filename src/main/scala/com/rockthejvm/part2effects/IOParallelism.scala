package com.rockthejvm.part2effects

import cats.effect.{IO, IOApp}

object IOParallelism extends IOApp.Simple{

  // IDs are usually sequential
  val anjaliIO = IO(s"[${Thread.currentThread().getName}] Anjali")
  val adiIO = IO(s"[${Thread.currentThread().getName}] Adi")

  val composedIO = for {
    anj <- anjaliIO
    adi <- adiIO
  } yield s"$anj and $adi are our great children"

  // debug extension method
  import com.rockthejvm.utils._
  // mapN extension method
  import cats.syntax.apply._
  import cats.Parallel

  val meaningOfLife: IO[Int] = IO.delay(42)
  val favLang: IO[String] = IO.delay("scala")
  val goalInLife = (meaningOfLife.debug, favLang.debug).mapN((num, string) => s"my goal in life is $num and $string")

  // parallelism on IOs
  // convert a sequential IO to parallel IO
  val parIO1: IO.Par[Int] = Parallel[IO].parallel(meaningOfLife.debug)
  val parIO2: IO.Par[String] = Parallel[IO].parallel(favLang.debug)
  import cats.effect.implicits._
  val goalInLifeParallel: IO.Par[String] = (parIO1, parIO2).mapN((num, string) => s"my goal in life is $num and $string")
  // turn back to sequential
  val goalInLife_v2: IO[String] = Parallel[IO].sequential(goalInLifeParallel)

  //shorthand:
  import cats.syntax.parallel._
  val goalInLife_v3 = (meaningOfLife.debug, favLang.debug).parMapN((num, string) => s"my goal in life is $num and $string")

  // regarding failure
  val aFailure: IO[String] = IO.raiseError(new RuntimeException("I can't do this!"))
  // compose success + failure
  val parallelWithFailure = (meaningOfLife.debug, aFailure.debug).parMapN(_ + _)
  // compose failure + failure
  val anotherFailure: IO[String] = IO.raiseError(new RuntimeException("Second failure"))
  val twoFailures: IO[String] = (aFailure.debug, anotherFailure.debug).parMapN(_ + _)
  val twoFailuresDelayed: IO[String] = (IO(Thread.sleep(1000)) >> aFailure.debug, anotherFailure.debug).parMapN(_ + _)

  override def run: IO[Unit] =
    twoFailuresDelayed.debug.void
//    goalInLife_v2.map(println)
}
